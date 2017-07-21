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
, runFreer
, stepFreer
, freerSteps
, retract
, foldFreer
, cutoff
, wrap
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
  where go r = case r of
          Return a -> Return a
          Then r t -> Then (f r) (go . t)


data FreerF f a b where
  ReturnF :: a -> FreerF f a b
  ThenF :: f x -> (x -> b) -> FreerF f a b

liftFreerF :: f b -> FreerF f a b
liftFreerF action = action `ThenF` id
{-# INLINE liftFreerF #-}

hoistFreerF :: (forall a. f a -> g a) -> FreerF f b c -> FreerF g b c
hoistFreerF f r = case r of
  ReturnF a -> ReturnF a
  ThenF r t -> ThenF (f r) t


iter :: Functor f => (f a -> a) -> Freer f a -> a
iter algebra = iterFreer ((algebra .) . flip fmap)

iterA :: (Functor f, Applicative m) => (f (m a) -> m a) -> Freer f a -> m a
iterA algebra = iterFreerA ((algebra .) . flip fmap)

iterFreer :: (forall x. f x -> (x -> a) -> a) -> Freer f a -> a
iterFreer algebra = cata $ \ r -> case r of
  ReturnF result -> result
  ThenF action continue -> algebra action continue
{-# INLINE iterFreer #-}

iterFreerA :: Applicative m => (forall x. f x -> (x -> m a) -> m a) -> Freer f a -> m a
iterFreerA algebra r = iterFreer algebra (fmap pure r)
{-# INLINE iterFreerA #-}


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
retract = iterFreerA (>>=)
{-# INLINE retract #-}

foldFreer :: Monad m => (forall x. f x -> m x) -> Freer f a -> m a
foldFreer f = retract . hoistFreer f
{-# INLINE foldFreer #-}


cutoff :: Integer -> Freer f a -> Freer f (Either (Freer f a) a)
cutoff n r | n <= 0 = return (Left r)
cutoff n (Then a f) = Then a (cutoff (pred n) . f)
cutoff _ r = Right <$> r


-- Instances

instance Functor (Freer f) where
  fmap f = go
    where go r = case r of
            Return a -> Return (f a)
            Then r t -> Then r (go . t)
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
    where go d r = case r of
            Return a -> showsUnaryWith sp "Return" d a
            Then r t -> showsBinaryWith (liftShowsPrec ((. t) . go) (liftShowList sp sl . fmap t)) (const showString) "Then" d r "_"

instance (Show1 f, Show a) => Show (Freer f a) where
  showsPrec = liftShowsPrec showsPrec showList

instance Eq1 f => Eq1 (Freer f) where
  liftEq eqA = go
    where go r s = case (r, s) of
            (Return a1, Return a2) -> eqA a1 a2
            (Then r1 t1, Then r2 t2) -> liftEq (\ x1 x2 -> go (t1 x1) (t2 x2)) r1 r2
            _ -> False

instance (Eq1 f, Eq a) => Eq (Freer f a) where
  (==) = liftEq (==)


instance Functor (FreerF f a) where
  fmap _ (ReturnF a) = ReturnF a
  fmap f (ThenF r g) = ThenF r (f . g)
  {-# INLINE fmap #-}

instance Bifunctor (FreerF f) where
  bimap f g r = case r of
    ReturnF a -> ReturnF (f a)
    ThenF r t -> ThenF r (g . t)
  {-# INLINE bimap #-}


instance Foldable f => Foldable (FreerF f a) where
  foldMap f g = case g of
    ReturnF _ -> mempty
    ThenF r t -> foldMap (f . t) r
  {-# INLINE foldMap #-}

instance Traversable f => Traversable (FreerF f a) where
  traverse f g = case g of
    ReturnF a -> pure (ReturnF a)
    ThenF r t -> liftFreerF <$> traverse (f . t) r
  {-# INLINE traverse #-}


instance Eq1 f => Eq2 (FreerF f) where
  liftEq2 eqA eqB f1 f2 = case (f1, f2) of
    (ReturnF a1, ReturnF a2) -> eqA a1 a2
    (ThenF r1 t1, ThenF r2 t2) -> liftEq (\ x1 x2 -> eqB (t1 x1) (t2 x2)) r1 r2
    _ -> False

instance (Eq1 f, Eq a) => Eq1 (FreerF f a) where
  liftEq = liftEq2 (==)

instance (Eq1 f, Eq a, Eq b) => Eq (FreerF f a b) where
  (==) = liftEq (==)


instance Show1 f => Show2 (FreerF f) where
  liftShowsPrec2 sp1 _ sp2 sa2 d f = case f of
    ReturnF a -> showsUnaryWith sp1 "ReturnF" d a
    ThenF r t -> showsBinaryWith (liftShowsPrec (\ i -> sp2 i . t) (sa2 . fmap t)) (const showString) "ThenF" d r "_"

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
