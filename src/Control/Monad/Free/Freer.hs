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
  Pure :: a -> Freer f a
  Free :: f x -> (x -> Freer f a) -> Freer f a

iter :: Functor f => (f a -> a) -> Freer f a -> a
iter algebra = iterFreer ((algebra .) . flip fmap)

iterA :: (Functor f, Applicative m) => (f (m a) -> m a) -> Freer f a -> m a
iterA algebra = cata $ \ r -> case r of
  T.Pure a -> pure a
  T.Free r t -> algebra (t <$> r)

iterFreer :: (forall x. f x -> (x -> a) -> a) -> Freer f a -> a
iterFreer algebra = cata $ \ r -> case r of
  T.Pure a -> a
  T.Free r t -> algebra r t

iterFreerA :: Applicative m => (forall x. f x -> (x -> m a) -> m a) -> Freer f a -> m a
iterFreerA algebra = cata $ \ r -> case r of
  T.Pure a -> pure a
  T.Free r t -> algebra r t

hoistFreer :: (forall a. f a -> g a) -> Freer f b -> Freer g b
hoistFreer f = go
  where go r = case r of
          Pure a -> Pure a
          Free r t -> Free (f r) (go . t)

liftF :: f a -> Freer f a
liftF = flip Free pure


-- Instances

instance Functor (Freer f) where
  fmap f = go
    where go r = case r of
            Pure a -> Pure (f a)
            Free r t -> Free r (go . t)

instance Applicative (Freer f) where
  pure = Pure
  g <*> a = case g of
    Pure f -> fmap f a
    Free r t -> Free r ((<*> a) . t)

instance Monad (Freer f) where
  return = pure
  g >>= f = case g of
    Pure a -> f a
    Free r t -> Free r (t >=> f)

instance MonadFree f (Freer f) where
  wrap = flip Free id


instance Foldable f => Foldable (Freer f) where
  foldMap f = go
    where go r = case r of
            Pure a -> f a
            Free r t -> foldMap (go . t) r

instance Traversable f => Traversable (Freer f) where
  traverse f = go
    where go g = case g of
            Pure a -> pure <$> f a
            Free r t -> wrap <$> traverse (go . t) r


type instance Base (Freer f a) = T.FreerF f a

instance Recursive (Freer f a) where
  project (Pure a) = T.Pure a
  project (Free r t) = T.Free r t

instance Corecursive (Freer f a) where
  embed (T.Pure a) = Pure a
  embed (T.Free r t) = Free r t


instance Show1 f => Show1 (Freer f) where
  liftShowsPrec sp sl = go
    where go d r = case r of
            Pure a -> showsUnaryWith sp "Pure" d a
            Free r t -> showsBinaryWith (const showString) (liftShowsPrec (\ i -> go i . t) (liftShowList sp sl . fmap t)) "Freer" d "id" r

instance (Show1 f, Show a) => Show (Freer f a) where
  showsPrec = liftShowsPrec showsPrec showList

instance Eq1 f => Eq1 (Freer f) where
  liftEq eqA = go
    where go r s = case (r, s) of
            (Pure a1, Pure a2) -> eqA a1 a2
            (Free r1 t1, Free r2 t2) -> liftEq (\ x1 x2 -> go (t1 x1) (t2 x2)) r1 r2
            _ -> False

instance (Eq1 f, Eq a) => Eq (Freer f a) where
  (==) = liftEq (==)

instance Listable1 f => Listable1 (Freer f) where
  liftTiers t1 = go where go = liftCons1 t1 Pure \/ liftCons1 (liftTiers go) wrap

instance (Listable a, Listable1 f) => Listable (Freer f a) where
  tiers = liftTiers tiers
