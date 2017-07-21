{-# LANGUAGE FlexibleInstances, GADTs, MultiParamTypeClasses, RankNTypes, ScopedTypeVariables, TypeFamilies #-}
module Control.Monad.Free.Freer
( Freer(..)
, liftF
, hoistFreer
, FreerF(..)
, liftFreerF
, hoistFreerF
, iter
, iterA
, iterFreer
, iterFreerA
, iterLookahead
, runFreer
, stepFreer
, freerSteps
, retract
, cutoff
, foldFreer
, wrap
) where

import Control.Monad ((<=<))
import Control.Monad.Free.Class hiding (liftF)
import Data.Bifunctor
import Data.Functor.Classes
import Data.Functor.Foldable
import Text.Show (showListWith)

data Freer f a where
  Return :: a -> Freer f a
  Map :: (b -> a) -> f b -> Freer f a
  Seq :: (c -> b -> a) -> f c -> Freer f b -> Freer f a
  Then :: f b -> (b -> Freer f a) -> Freer f a

infixl 1 `Then`

-- | Lift a value in any functor into a 'Freer' monad.
liftF :: f a -> Freer f a
liftF action = Map id action
{-# INLINE liftF #-}

hoistFreer :: forall f g a . (forall x. f x -> g x) -> Freer f a -> Freer g a
hoistFreer f = go
  where go :: forall a . Freer f a -> Freer g a
        go (Return a) = Return a
        go (Map g a) = Map g (f a)
        go (Seq g a b) = Seq g (f a) (go b)
        go (Then r t) = Then (f r) (go . t)


data FreerF f a r where
  ReturnF :: a -> FreerF f a r
  MapF :: (b -> a) -> f b -> FreerF f a r
  ThenF :: f b -> (b -> r) -> FreerF f a r

liftFreerF :: f b -> FreerF f a b
liftFreerF action = action `ThenF` id
{-# INLINE liftFreerF #-}

hoistFreerF :: (forall a. f a -> g a) -> FreerF f b c -> FreerF g b c
hoistFreerF f r = case r of
  ReturnF a -> ReturnF a
  MapF g a -> MapF g (f a)
  ThenF r t -> ThenF (f r) t


iter :: Functor f => (f a -> a) -> Freer f a -> a
iter algebra = iterFreer ((algebra .) . flip fmap)

iterA :: (Functor f, Applicative m) => (f (m a) -> m a) -> Freer f a -> m a
iterA algebra = iterFreerA ((algebra .) . flip fmap)

iterFreer :: (forall x. f x -> (x -> a) -> a) -> Freer f a -> a
iterFreer algebra = cata $ \ r -> case r of
  ReturnF result -> result
  MapF transform action -> algebra action transform
  ThenF action continue -> algebra action continue
{-# INLINE iterFreer #-}

iterFreerA :: Applicative m => (forall x. f x -> (x -> m a) -> m a) -> Freer f a -> m a
iterFreerA algebra r = iterFreer algebra (fmap pure r)
{-# INLINE iterFreerA #-}

iterLookahead :: forall f a. (forall x y. f x -> Maybe (Freer f y) -> (x -> a) -> a) -> Freer f a -> a
iterLookahead algebra = go
  where go :: Freer f a -> a
        go (Return a) = a
        go (Map f a) = algebra a Nothing f
        go (Seq f a b) = algebra a (Just b) (go . flip fmap b . f)
        go (Then a f) = algebra a Nothing (go . f)
{-# INLINE iterLookahead #-}


-- | Run a program to completion by repeated refinement, and return its result.
runFreer :: forall f result
         .  (forall x. f x -> Freer f x)
         -> Freer f result
         -> result
runFreer refine = go
  where go :: Freer f x -> x
        go = iterFreer (flip ($) . go . refine)

-- | Run a single step of a program by refinement, returning 'Either' its @result@ or the next step.
stepFreer :: (forall x. f x -> Freer f x)
          -> Freer f result
          -> Either result (Freer f result)
stepFreer refine = go
  where go r = case r of
          Return a -> Left a
          Map f step -> Right (f <$> refine step)
          Seq f a b -> go (liftAp f a b)
          step `Then` yield -> Right (refine step >>= yield)

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


retract :: Monad m => Freer m a -> m a
retract r = case r of
  Return a -> return a
  Map f a -> fmap f a
  Seq f a b -> f <$> a <*> retract b
  Then a f -> a >>= retract . f

cutoff :: Integer -> Freer f a -> Freer f (Either (Freer f a) a)
cutoff n r | n <= 0 = return (Left r)
cutoff n (Seq f a b) = Seq (\ a -> bimap (fmap (f a)) (f a)) a (cutoff (pred n) b)
cutoff n (Then a f) = Then a (cutoff (pred n) . f)
cutoff _ r = Right <$> r

foldFreer :: Monad m => (forall x. f x -> m x) -> Freer f a -> m a
foldFreer f = retract . hoistFreer f


-- | Rewrite 'Applicative' parameters  as 'Then'. Typically used for normalization or linearization.
liftAp :: (a -> b -> c) -> f a -> Freer f b -> Freer f c
liftAp f a b = Then a (flip fmap b . f)
{-# INLINE liftAp #-}


-- Instances

instance Functor (Freer f) where
  fmap f = go
    where go (Return a) = Return (f a)
          go (Map g a) = Map (f . g) a
          go (Seq g a b) = Seq ((f .) . g) a b
          go (Then r t) = Then r (go . t)
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
    where go d (Return a) = showsUnaryWith sp "Return" d a
          go d (Map f a) = showsBinaryWith (const showString) (liftShowsPrec ((. f) . sp) (sl . fmap f)) "Map" d "_" a
          go d (Seq _ a b) = showsBinaryWith (liftShowsPrec (showsPrec_ "_a") (showList_ "_a")) (liftShowsPrec (showsPrec_ "_b") (showList_ "_b")) "Seq _" d a b
          go d (Then r t) = showsBinaryWith (liftShowsPrec ((. t) . go) (liftShowList sp sl . fmap t)) (const showString) "Then" d r "_"
          showsPrec_ :: String -> Int -> a -> ShowS
          showsPrec_ s _ _ = showString s
          showList_ :: String -> [a] -> ShowS
          showList_ s = showListWith (showsPrec_ s 0)

instance (Show1 f, Show a) => Show (Freer f a) where
  showsPrec = liftShowsPrec showsPrec showList

instance Eq1 f => Eq1 (Freer f) where
  liftEq eqA = go
    where go r s = case (r, s) of
            (Return a1, Return a2) -> eqA a1 a2
            (Map f1 a1, Map f2 a2) -> liftEq (\ a b -> f1 a `eqA` f2 b) a1 a2
            (Seq f1 a1 b1, Seq f2 a2 b2) -> go (liftAp f1 a1 b1) (liftAp f2 a2 b2)
            (Then r1 t1, Then r2 t2) -> liftEq (\ x1 x2 -> go (t1 x1) (t2 x2)) r1 r2
            _ -> False

instance (Eq1 f, Eq a) => Eq (Freer f a) where
  (==) = liftEq (==)


instance Functor (FreerF f a) where
  fmap _ (ReturnF a) = ReturnF a
  fmap _ (MapF f a) = MapF f a
  fmap f (ThenF r g) = ThenF r (f . g)
  {-# INLINE fmap #-}

instance Bifunctor (FreerF f) where
  bimap f _ (ReturnF a) = ReturnF (f a)
  bimap f _ (MapF g a) = MapF (f . g) a
  bimap _ g (ThenF r t) = ThenF r (g . t)
  {-# INLINE bimap #-}


instance Foldable f => Foldable (FreerF f a) where
  foldMap f g = case g of
    ReturnF _ -> mempty
    MapF _ _ -> mempty
    ThenF r t -> foldMap (f . t) r
  {-# INLINE foldMap #-}

instance Traversable f => Traversable (FreerF f a) where
  traverse f = go
    where go (ReturnF a) = pure (ReturnF a)
          go (MapF g a) = MapF g <$> traverse pure a
          go (ThenF r t) = liftFreerF <$> traverse (f . t) r
  {-# INLINE traverse #-}


instance Eq1 f => Eq2 (FreerF f) where
  liftEq2 eqA eqB f1 f2 = case (f1, f2) of
    (ReturnF a1, ReturnF a2) -> eqA a1 a2
    (MapF f1 a1, MapF f2 a2) -> liftEq (\ a b -> f1 a `eqA` f2 b) a1 a2
    (ThenF r1 t1, ThenF r2 t2) -> liftEq (\ x1 x2 -> eqB (t1 x1) (t2 x2)) r1 r2
    _ -> False

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
