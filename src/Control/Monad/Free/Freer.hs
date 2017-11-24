{-# LANGUAGE FlexibleInstances, GADTs, MultiParamTypeClasses, RankNTypes, ScopedTypeVariables, TypeFamilies #-}
module Control.Monad.Free.Freer
( Freer(..)
, wrap
, liftF
, hoistFreer
, FreerF(..)
, liftFreerF
, hoistFreerF
, iter
, iterA
, iterFreer
, iterFreerA
, runFreer
, runFreerM
, stepFreer
, freerSteps
, retract
, foldFreer
, cutoff
) where

import Control.Monad ((<=<))
import Control.Monad.Free.Class hiding (liftF)
import Data.Bifunctor
import Data.Functor.Classes
import Data.Functor.Foldable

data Freer f a where
  Return :: a -> Freer f a
  Then :: f x -> (x -> Freer f a) -> Freer f a

infixl 1 `Then`

liftF :: f a -> Freer f a
liftF action = action `Then` return
{-# INLINE liftF #-}

hoistFreer :: (forall a. f a -> g a) -> Freer f b -> Freer g b
hoistFreer f = go
  where go (Return result) = Return result
        go (Then step yield) = Then (f step) (go . yield)
        {-# INLINE go #-}
{-# INLINE hoistFreer #-}


data FreerF f a b where
  ReturnF :: a -> FreerF f a b
  ThenF :: f x -> (x -> b) -> FreerF f a b

liftFreerF :: f b -> FreerF f a b
liftFreerF action = action `ThenF` id
{-# INLINE liftFreerF #-}

hoistFreerF :: (forall a. f a -> g a) -> FreerF f b c -> FreerF g b c
hoistFreerF _ (ReturnF result) = ReturnF result
hoistFreerF f (ThenF step yield) = ThenF (f step) yield


-- | Tear down a 'Freer' 'Monad' using iteration.
--
--   This is analogous to 'cata' where the 'Return'ed values are placeholders for the result of the computation.
iter :: Functor f => (f a -> a) -> Freer f a -> a
iter algebra = iterFreer (\ yield -> algebra . fmap yield)

iterA :: (Functor f, Applicative m) => (f (m a) -> m a) -> Freer f a -> m a
iterA algebra = iterFreerA (\ yield -> algebra . fmap yield)

iterFreer :: (forall x. (x -> a) -> f x -> a) -> Freer f a -> a
iterFreer algebra = go
  where go (Return result) = result
        go (Then action continue) = algebra (go . continue) action
        {-# INLINE go #-}
{-# INLINE iterFreer #-}

iterFreerA :: Applicative m => (forall x. (x -> m a) -> f x -> m a) -> Freer f a -> m a
iterFreerA algebra r = iterFreer algebra (fmap pure r)
{-# INLINE iterFreerA #-}


-- | Run a program to completion by repeated refinement, and return its result.
runFreer :: forall f result
         .  (forall x. f x -> Freer f x)
         -> Freer f result
         -> result
runFreer refine = go
  where go :: Freer f x -> x
        go = iterFreer ((. (go . refine)) . ($))
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
        go = iterFreer ((. (go . refine)) . (=<<))
{-# INLINE runFreerM #-}

-- | Run a single step of a program by refinement, returning 'Either' its @result@ or the next step.
stepFreer :: (forall x. f x -> Freer f x)
          -> Freer f result
          -> Either result (Freer f result)
stepFreer refine = go
  where go (Return a) = Left a
        go (step `Then` yield) = Right (refine step >>= yield)

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
retract = iterFreerA (=<<)
{-# INLINE retract #-}

foldFreer :: Monad m => (forall x. f x -> m x) -> Freer f a -> m a
foldFreer f = retract . hoistFreer f
{-# INLINE foldFreer #-}


cutoff :: Integer -> Freer f a -> Freer f (Either (Freer f a) a)
cutoff n r | n <= 0 = return (Left r)
cutoff n (Then step yield) = Then step (cutoff (pred n) . yield)
cutoff _ r = Right <$> r
{-# INLINE cutoff #-}


-- Instances

instance Functor (Freer f) where
  fmap f = go
    where go (Return result) = Return (f result)
          go (Then step yield) = Then step (go . yield)
          {-# INLINE go #-}
  {-# INLINE fmap #-}

instance Applicative (Freer f) where
  pure = Return
  {-# INLINE pure #-}

  Return f <*> param = fmap f param
  Then action yield <*> param = Then action ((<*> param) . yield)
  {-# INLINE (<*>) #-}

  Return _ *> a = a
  Then r f *> a = Then r ((*> a) . f)
  {-# INLINE (*>) #-}

  Return a <* b = b *> Return a
  Then r f <* a = Then r ((<* a) . f)
  {-# INLINE (<*) #-}

instance Monad (Freer f) where
  return = pure
  {-# INLINE return #-}

  Return a >>= f = f a
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
          go (Then r t) = foldMap (go . t) r
  {-# INLINE foldMap #-}

instance Traversable f => Traversable (Freer f) where
  traverse f = go
    where go (Return a) = pure <$> f a
          go (Then r t) = wrap <$> traverse (go . t) r
  {-# INLINE traverse #-}


instance Show1 f => Show1 (Freer f) where
  liftShowsPrec sp sl = go
    where go d (Return a) = showsUnaryWith sp "Return" d a
          go d (Then step yield) = showsBinaryWith (liftShowsPrec ((. yield) . go) (liftShowList sp sl . fmap yield)) (const showString) "Then" d step "_"

instance (Show1 f, Show a) => Show (Freer f a) where
  showsPrec = liftShowsPrec showsPrec showList

instance Eq1 f => Eq1 (Freer f) where
  liftEq eqResult = go
    where go (Return result1) (Return result2) = eqResult result1 result2
          go (Then step1 yield1) (Then step2 yield2) = liftEq (\ x1 x2 -> go (yield1 x1) (yield2 x2)) step1 step2
          go _ _ = False

instance (Eq1 f, Eq a) => Eq (Freer f a) where
  (==) = liftEq (==)


instance Functor (FreerF f a) where
  fmap _ (ReturnF a) = ReturnF a
  fmap f (ThenF r g) = ThenF r (f . g)
  {-# INLINE fmap #-}

instance Bifunctor (FreerF f) where
  bimap f _ (ReturnF result) = ReturnF (f result)
  bimap _ g (ThenF step yield) = ThenF step (g . yield)
  {-# INLINE bimap #-}


instance Foldable f => Foldable (FreerF f a) where
  foldMap _ (ReturnF _) = mempty
  foldMap f (ThenF step yield) = foldMap (f . yield) step
  {-# INLINE foldMap #-}

instance Traversable f => Traversable (FreerF f a) where
  traverse _ (ReturnF result) = pure (ReturnF result)
  traverse f (ThenF step yield) = liftFreerF <$> traverse (f . yield) step
  {-# INLINE traverse #-}


instance Eq1 f => Eq2 (FreerF f) where
  liftEq2 eqResult eqRecur (ReturnF result1) (ReturnF result2) = eqResult result1 result2
  liftEq2 _ eqRecur (ThenF step1 yield1) (ThenF step2 yield2) = liftEq (\ x1 x2 -> eqRecur (yield1 x1) (yield2 x2)) step1 step2
  liftEq2 _ _ _ _ = False

instance (Eq1 f, Eq a) => Eq1 (FreerF f a) where
  liftEq = liftEq2 (==)

instance (Eq1 f, Eq a, Eq b) => Eq (FreerF f a b) where
  (==) = liftEq (==)


instance Show1 f => Show2 (FreerF f) where
  liftShowsPrec2 sp1 _ _ _ d (ReturnF result) = showsUnaryWith sp1 "ReturnF" d result
  liftShowsPrec2 sp1 _ sp2 sa2 d (ThenF step yield) = showsBinaryWith (liftShowsPrec ((. yield) . sp2) (sa2 . fmap yield)) (const showString) "ThenF" d step "_"

instance (Show1 f, Show a) => Show1 (FreerF f a) where
  liftShowsPrec = liftShowsPrec2 showsPrec showList

instance (Show1 f, Show a, Show b) => Show (FreerF f a b) where
  showsPrec = liftShowsPrec showsPrec showList


type instance Base (Freer f a) = FreerF f a

instance Recursive (Freer f a) where
  project (Return a) = ReturnF a
  project (Then r t) = ThenF r t
  {-# INLINE project #-}

instance Corecursive (Freer f a) where
  embed (ReturnF a) = Return a
  embed (ThenF r t) = Then r t
  {-# INLINE embed #-}
