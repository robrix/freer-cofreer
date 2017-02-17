{-# LANGUAGE FlexibleInstances, GADTs, MultiParamTypeClasses, RankNTypes, TypeFamilies #-}
module Control.Monad.Free.Freer
( Freer(..)
, iter
, iterA
, iterFreer
, iterFreerA
, hoistFreer
, liftF
, wrap
) where

import Control.Monad ((>=>))
import Control.Monad.Free.Class hiding (liftF)
import qualified Control.Monad.Trans.Free.Freer as T
import Data.Bifunctor
import Data.Functor.Classes
import Data.Functor.Foldable
import Data.Functor.Listable

data Freer f a where
  Return :: a -> Freer f a
  Then :: f x -> (x -> Freer f a) -> Freer f a

iter :: Functor f => (f a -> a) -> Freer f a -> a
iter algebra = iterFreer ((algebra .) . flip fmap)

iterA :: (Functor f, Applicative m) => (f (m a) -> m a) -> Freer f a -> m a
iterA algebra = cata $ \ r -> case r of
  T.Return a -> pure a
  T.Then r t -> algebra (t <$> r)

iterFreer :: (forall x. f x -> (x -> a) -> a) -> Freer f a -> a
iterFreer algebra = cata $ \ r -> case r of
  T.Return a -> a
  T.Then r t -> algebra r t

iterFreerA :: Applicative m => (forall x. f x -> (x -> m a) -> m a) -> Freer f a -> m a
iterFreerA algebra = cata $ \ r -> case r of
  T.Return a -> pure a
  T.Then r t -> algebra r t

hoistFreer :: (forall a. f a -> g a) -> Freer f b -> Freer g b
hoistFreer f = go
  where go r = case r of
          Return a -> Return a
          Then r t -> Then (f r) (go . t)

liftF :: f a -> Freer f a
liftF = flip Then pure


-- Instances

instance Functor (Freer f) where
  fmap f = go
    where go r = case r of
            Return a -> Return (f a)
            Then r t -> Then r (go . t)

instance Applicative (Freer f) where
  pure = Return
  g <*> a = case g of
    Return f -> fmap f a
    Then r t -> Then r ((<*> a) . t)

instance Monad (Freer f) where
  return = pure
  g >>= f = case g of
    Return a -> f a
    Then r t -> Then r (t >=> f)

instance MonadFree f (Freer f) where
  wrap = flip Then id


instance Foldable f => Foldable (Freer f) where
  foldMap f = go
    where go r = case r of
            Return a -> f a
            Then r t -> foldMap (go . t) r

instance Traversable f => Traversable (Freer f) where
  traverse f = go
    where go g = case g of
            Return a -> pure <$> f a
            Then r t -> wrap <$> traverse (go . t) r


type instance Base (Freer f a) = T.FreerF f a

instance Recursive (Freer f a) where
  project (Return a) = T.Return a
  project (Then r t) = T.Then r t

instance Corecursive (Freer f a) where
  embed (T.Return a) = Return a
  embed (T.Then r t) = Then r t


instance Show1 f => Show1 (Freer f) where
  liftShowsPrec sp sl = go
    where go d r = case r of
            Return a -> showsUnaryWith sp "Return" d a
            Then r t -> showsBinaryWith (liftShowsPrec (\ i -> go i . t) (liftShowList sp sl . fmap t)) (const showString) "Then" d r "id"

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

instance Listable1 f => Listable1 (Freer f) where
  liftTiers t1 = go where go = liftCons1 t1 Return \/ liftCons1 (liftTiers go) wrap

instance (Listable a, Listable1 f) => Listable (Freer f a) where
  tiers = liftTiers tiers
