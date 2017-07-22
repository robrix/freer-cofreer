{-# LANGUAGE FlexibleInstances, GADTs, MultiParamTypeClasses, RankNTypes, ScopedTypeVariables, TypeFamilies #-}
module Control.Monad.Free.Freer
( Freer(..)
, wrap
, liftF
, hoistFreer
, FreerF(..)
, liftFreerF
, hoistFreerF
-- Iteration
, iter
, iterA
, iterFreer
, iterFreerA
, iterLookahead
-- Iteration by refinement
, runFreer
, runFreerM
, stepFreer
, freerSteps
-- Monadic reduction
, retract
, foldFreer
-- Bounding
, cutoff
) where

import Control.Monad ((<=<))
import Control.Monad.Free.Class hiding (liftF)
import Data.Bifunctor
import Data.Functor.Classes
import Data.Functor.Foldable
import Text.Show (showListWith)

-- | 'Freer' lifts any @f :: * -> *@ into a value with unconstrained 'Functor', 'Applicative', and 'Monad' instances.
data Freer f a where
  -- | A pure computation.
  Return :: a -> Freer f a
  -- | Mapping over a value in @f@. This can be understood as storing the parameters to 'fmap'.
  Map :: (b -> a) -> f b -> Freer f a
  -- | Sequencing of values in @f@. This can be understood as storing the parameters to 'liftA2'.
  Seq :: (c -> b -> a) -> f c -> Freer f b -> Freer f a
  -- | 'Monad'ic binding of values in @f@. This can be understood as storing the parameters to '>>='.
  Then :: f b -> (b -> Freer f a) -> Freer f a

infixl 1 `Then`

-- | Lift a value in any functor into a 'Freer' monad.
liftF :: f a -> Freer f a
liftF action = Map id action
{-# INLINE liftF #-}

-- | Lift a natural transformation from @f@ to @g@ into a natural transformation from @'Freer' f@ to @'Freer' g@.
hoistFreer :: forall f g a . (forall x. f x -> g x) -> Freer f a -> Freer g a
hoistFreer f = go
  where go :: forall a . Freer f a -> Freer g a
        go (Return result)     = Return result
        go (Map g step)        = Map g (f step)
        go (Seq g step1 step2) = Seq g (f step1) (go step2)
        go (Then step yield)   = Then (f step) (go . yield)
        {-# INLINE go #-}
{-# INLINE hoistFreer #-}


data FreerF f a r where
  ReturnF :: a -> FreerF f a r
  MapF :: (b -> a) -> f b -> FreerF f a r
  ThenF :: f b -> (b -> r) -> FreerF f a r

liftFreerF :: f b -> FreerF f a b
liftFreerF action = action `ThenF` id
{-# INLINE liftFreerF #-}

hoistFreerF :: (forall a. f a -> g a) -> FreerF f b c -> FreerF g b c
hoistFreerF _ (ReturnF result) = ReturnF result
hoistFreerF f (MapF g step)  = MapF g (f step)
hoistFreerF f (ThenF step yield) = ThenF (f step) yield


iter :: Functor f => (f a -> a) -> Freer f a -> a
iter algebra = iterFreer ((algebra .) . flip fmap)

iterA :: (Functor f, Applicative m) => (f (m a) -> m a) -> Freer f a -> m a
iterA algebra = iterFreerA ((algebra .) . flip fmap)

iterFreer :: (forall x. f x -> (x -> a) -> a) -> Freer f a -> a
iterFreer algebra = go
  where go (Return result) = result
        go (Map transform action) = algebra action transform
        go (Seq f action rest) = go (liftAp f action rest)
        go (Then action continue) = algebra action (go . continue)
        {-# INLINE go #-}
{-# INLINE iterFreer #-}

iterFreerA :: Applicative m => (forall x. f x -> (x -> m a) -> m a) -> Freer f a -> m a
iterFreerA algebra r = iterFreer algebra (fmap pure r)
{-# INLINE iterFreerA #-}

iterLookahead :: forall f a. (forall x y. f x -> Maybe (Freer f y) -> (x -> a) -> a) -> Freer f a -> a
iterLookahead algebra = go
  where go :: Freer f a -> a
        go (Return a)  = a
        go (Map f a)   = algebra a Nothing f
        go (Seq f a b) = algebra a (Just b) (go . flip fmap b . f)
        go (Then a f)  = algebra a Nothing (go . f)
{-# INLINE iterLookahead #-}


-- | Run a program to completion by repeated refinement, and return its result.
runFreer :: forall f result
         .  (forall x. f x -> Freer f x)
         -> Freer f result
         -> result
runFreer refine = go
  where go :: Freer f x -> x
        go = iterFreer (flip ($) . go . refine)
        {-# INLINE go #-}
{-# INLINE runFreer #-}

-- | Run a program to completion by repeated refinement in some 'Monad'ic context, and return its result.
runFreerM :: forall f m result
          .  Monad m
          => (forall x. f x -> Freer f (m x))
          -> Freer f result
          -> m result
runFreerM refine r = go (fmap pure r)
  where go :: Freer f (m x) -> m x
        go = iterFreer ((>>=) . go . refine)
{-# INLINE runFreerM #-}

-- | Run a single step of a program by refinement, returning 'Either' its @result@ or the next step.
stepFreer :: (forall x. f x -> Freer f x)
          -> Freer f result
          -> Either result (Freer f result)
stepFreer refine = go
  where go (Return a)          = Left a
        go (Map f step)        = Right (f <$> refine step)
        go (Seq f step1 step2) = go (liftAp f step1 step2)
        go (step `Then` yield) = Right (refine step >>= yield)
{-# INLINE stepFreer #-}


-- | Run a program to completion by repeated refinement, returning the list of steps up to and including the final result.
--
--   The steps are unfolded lazily, making this suitable for stepwise evaluation of nonterminating programs.
freerSteps :: (forall x. f x -> Freer f x)
           -> Freer f result
           -> [Freer f result]
freerSteps refine = go
  where go r = case stepFreer refine r of
          Left a -> [Return a]
          Right step -> step : go step


-- | Sequence a 'Freer' action over an underlying 'Monad' @m@.
retract :: Monad m => Freer m a -> m a
retract (Return a) = return a
retract (Map f action) = f <$> action
retract (Seq f action1 action2) = f <$> action1 <*> retract action2
retract (action `Then` yield) = action >>= retract . yield
{-# INLINE retract #-}

-- | Fold a 'Freer' action by mapping its steps onto some 'Monad' @m@.
--
-- > foldFreer f = retract . hoistFreer f
foldFreer :: Monad m => (forall x. f x -> m x) -> Freer f a -> m a
foldFreer f r = retract (hoistFreer f r)
{-# INLINE foldFreer #-}


cutoff :: Integer -> Freer f a -> Freer f (Either (Freer f a) a)
cutoff n r | n <= 0 = return (Left r)
cutoff n (Seq f step1 step2) = Seq (\ a -> bimap (fmap (f a)) (f a)) step1 (cutoff (pred n) step2)
cutoff n (Then step yield) = Then step (cutoff (pred n) . yield)
cutoff _ r = Right <$> r
{-# INLINE cutoff #-}


-- | Rewrite 'Applicative' parameters  as 'Then'. Typically used for normalization or linearization.
liftAp :: (a -> b -> c) -> f a -> Freer f b -> Freer f c
liftAp f a b = Then a (flip fmap b . f)
{-# INLINE liftAp #-}


-- Instances

instance Functor (Freer f) where
  fmap f = go
    where go (Return result) = Return (f result)
          go (Map g step) = Map (f . g) step
          go (Seq g step1 step2) = Seq ((f .) . g) step1 step2
          go (Then step yield) = Then step (go . yield)
          {-# INLINE go #-}
  {-# INLINE fmap #-}

instance Applicative (Freer f) where
  pure = Return
  {-# INLINE pure #-}

  Return f <*> param = fmap f param
  Map f a <*> b = Seq f a b
  Seq f a b <*> c = Seq (uncurry . f) a ((,) <$> b <*> c)
  Then action yield <*> param = Then action ((<*> param) . yield)
  {-# INLINE (<*>) #-}

  Return _ *> a = a
  Map _ a *> b = Seq (flip const) a b
  Seq _ a b *> c = Seq (flip const) a (b *> c)
  Then r f *> a = Then r ((*> a) . f)
  {-# INLINE (*>) #-}

  Return a <* b = b *> Return a
  Map f a <* b = Seq (const . f) a b
  Seq f a b <* c = Seq f a (b <* c)
  Then r f <* a = Then r ((<* a) . f)
  {-# INLINE (<*) #-}

instance Monad (Freer f) where
  return = pure
  {-# INLINE return #-}

  Return a >>= f = f a
  Map f r >>= g = Then r (g . f)
  Seq f a b >>= g = liftAp f a b >>= g
  Then r f >>= g = Then r (g <=< f)
  {-# INLINE (>>=) #-}

  (>>) = (*>)
  {-# INLINE (>>) #-}

instance MonadFree f (Freer f) where
  wrap action = action `Then` id
  {-# INLINE wrap #-}


instance Foldable f => Foldable (Freer f) where
  foldMap f = go
    where go (Return a) = f a
          go (Map g a) = foldMap (f . g) a
          go (Seq g a b) = foldMap (foldMap ((f .) . g) a) b
          go (Then r t) = foldMap (go . t) r
  {-# INLINE foldMap #-}

instance Traversable f => Traversable (Freer f) where
  traverse f = go
    where go (Return a) = pure <$> f a
          go (Map g a) = liftF <$> traverse (f . g) a
          go (Seq g a b) = go (liftAp g a b)
          go (Then r t) = wrap <$> traverse (go . t) r
  {-# INLINE traverse #-}


instance Show1 f => Show1 (Freer f) where
  liftShowsPrec sp sl = go
    where go d (Return result) = showsUnaryWith sp "Return" d result
          go d (Map f step) = showsBinaryWith (const showString) (liftShowsPrec ((. f) . sp) (sl . fmap f)) "Map" d "_" step
          go d (Seq _ step1 step2) = showsBinaryWith (liftShowsPrec (showsPrec_ "_a") (showList_ "_a")) (liftShowsPrec (showsPrec_ "_b") (showList_ "_b")) "Seq _" d step1 step2
          go d (Then step yield) = showsBinaryWith (liftShowsPrec ((. yield) . go) (liftShowList sp sl . fmap yield)) (const showString) "Then" d step "_"
          showsPrec_ :: String -> Int -> a -> ShowS
          showsPrec_ s _ _ = showString s
          showList_ :: String -> [a] -> ShowS
          showList_ s = showListWith (showsPrec_ s 0)

instance (Show1 f, Show a) => Show (Freer f a) where
  showsPrec = liftShowsPrec showsPrec showList

instance Eq1 f => Eq1 (Freer f) where
  liftEq eqResult = go
    where go (Return result1) (Return result2) = eqResult result1 result2
          go (Map f1 step1) (Map f2 step2) = liftEq (\ a1 a2 -> f1 a1 `eqResult` f2 a2) step1 step2
          go (Seq f1 a1 b1) (Seq f2 a2 b2) = go (liftAp f1 a1 b1) (liftAp f2 a2 b2)
          go (Then step1 yield1) (Then step2 yield2) = liftEq (\ x1 x2 -> go (yield1 x1) (yield2 x2)) step1 step2
          go _ _ = False

instance (Eq1 f, Eq a) => Eq (Freer f a) where
  (==) = liftEq (==)


instance Functor (FreerF f a) where
  fmap _ (ReturnF a) = ReturnF a
  fmap _ (MapF f a) = MapF f a
  fmap f (ThenF r g) = ThenF r (f . g)
  {-# INLINE fmap #-}

instance Bifunctor (FreerF f) where
  bimap f _ (ReturnF result) = ReturnF (f result)
  bimap f _ (MapF g step) = MapF (f . g) step
  bimap _ g (ThenF step yield) = ThenF step (g . yield)
  {-# INLINE bimap #-}


instance Foldable f => Foldable (FreerF f a) where
  foldMap _ (ReturnF _) = mempty
  foldMap _ (MapF _ _) = mempty
  foldMap f (ThenF step yield) = foldMap (f . yield) step
  {-# INLINE foldMap #-}

instance Traversable f => Traversable (FreerF f a) where
  traverse _ (ReturnF result) = pure (ReturnF result)
  traverse _ (MapF g a) = MapF g <$> traverse pure a
  traverse f (ThenF step yield) = liftFreerF <$> traverse (f . yield) step
  {-# INLINE traverse #-}


instance Eq1 f => Eq2 (FreerF f) where
  liftEq2 eqResult _ (ReturnF result1) (ReturnF result2) = eqResult result1 result2
  liftEq2 eqResult _ (MapF f1 step1) (MapF f2 step2) = liftEq (\ a1 a2 -> f1 a1 `eqResult` f2 a2) step1 step2
  liftEq2 _ eqRecur (ThenF step1 yield1) (ThenF step2 yield2) = liftEq (\ x1 x2 -> eqRecur (yield1 x1) (yield2 x2)) step1 step2
  liftEq2 _ _ _ _ = False

instance (Eq1 f, Eq a) => Eq1 (FreerF f a) where
  liftEq = liftEq2 (==)

instance (Eq1 f, Eq a, Eq b) => Eq (FreerF f a b) where
  (==) = liftEq (==)


instance Show1 f => Show2 (FreerF f) where
  liftShowsPrec2 sp1 _ sp2 sa2 d f = case f of
    ReturnF a -> showsUnaryWith sp1 "ReturnF" d a
    MapF _ a -> showsBinaryWith (const showString) (liftShowsPrec (showsPrec_ "_a") (showList_ "_a")) "MapF" d "_" a
    ThenF r t -> showsBinaryWith (liftShowsPrec ((. t) . sp2) (sa2 . fmap t)) (const showString) "ThenF" d r "_"
    where showsPrec_ :: String -> Int -> a -> ShowS
          showsPrec_ s _ _ = showString s
          showList_ :: String -> [a] -> ShowS
          showList_ s = showListWith (showsPrec_ s 0)

instance (Show1 f, Show a) => Show1 (FreerF f a) where
  liftShowsPrec = liftShowsPrec2 showsPrec showList

instance (Show1 f, Show a, Show b) => Show (FreerF f a b) where
  showsPrec = liftShowsPrec showsPrec showList


type instance Base (Freer f a) = FreerF f a

instance Recursive (Freer f a) where
  project (Return a) = ReturnF a
  project (Map f a) = MapF f a
  project (Seq f a b) = project (liftAp f a b)
  project (Then r t) = ThenF r t
  {-# INLINE project #-}

instance Corecursive (Freer f a) where
  embed (ReturnF a) = Return a
  embed (MapF f a) = Map f a
  embed (ThenF r t) = Then r t
  {-# INLINE embed #-}
