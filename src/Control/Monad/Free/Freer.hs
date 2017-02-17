{-# LANGUAGE FlexibleInstances, GADTs, MultiParamTypeClasses, RankNTypes, TypeFamilies #-}
module Control.Monad.Free.Freer
( FreerF(..)
, Freer(..)
, liftFreerF
, iter
, iterA
, iterFreer
, iterFreerA
, hoistFreer
, hoistFreerF
, liftF
, wrap
) where

import Control.Monad ((>=>))
import Control.Monad.Free.Class hiding (liftF)
import Control.Monad.Trans.Free.Freer
import Data.Bifunctor
import Data.Functor.Classes
import Data.Functor.Foldable
import Data.Functor.Listable

newtype Freer f a = Freer { runFreer :: FreerF f a (Freer f a) }

iter :: Functor f => (f a -> a) -> Freer f a -> a
iter algebra = iterFreer ((algebra .) . flip fmap)

iterA :: (Functor f, Applicative m) => (f (m a) -> m a) -> Freer f a -> m a
iterA algebra = cata $ \ r -> case r of
  Pure a -> pure a
  Free r t -> algebra (t <$> r)

iterFreer :: (forall x. f x -> (x -> a) -> a) -> Freer f a -> a
iterFreer algebra = cata $ \ r -> case r of
  Pure a -> a
  Free r t -> algebra r t

iterFreerA :: Applicative m => (forall x. f x -> (x -> m a) -> m a) -> Freer f a -> m a
iterFreerA algebra = cata $ \ r -> case r of
  Pure a -> pure a
  Free r t -> algebra r t

hoistFreer :: (forall a. f a -> g a) -> Freer f b -> Freer g b
hoistFreer f = go
  where go = Freer . fmap go . hoistFreerF f . runFreer

liftF :: f a -> Freer f a
liftF = Freer . flip Free pure


-- Instances

instance Functor (Freer f) where
  fmap f = Freer . bimap f (fmap f) . runFreer

instance Applicative (Freer f) where
  pure = Freer . Pure
  Freer g <*> a = case g of
    Pure f -> fmap f a
    Free r t -> Freer (Free r ((<*> a) . t))

instance Monad (Freer f) where
  return = pure
  Freer g >>= f = case g of
    Pure a -> f a
    Free r t -> Freer (Free r (t >=> f))

instance MonadFree f (Freer f) where
  wrap = Freer . liftFreerF


instance Foldable f => Foldable (Freer f) where
  foldMap f = foldMap (foldMap f) . runFreer

instance Traversable f => Traversable (Freer f) where
  traverse f = go
    where go g = case runFreer g of
            Pure a -> pure <$> f a
            Free r t -> wrap <$> traverse (go . t) r


type instance Base (Freer f a) = FreerF f a

instance Recursive (Freer f a) where project = runFreer
instance Corecursive (Freer f a) where embed = Freer


instance Show1 f => Show1 (Freer f) where
  liftShowsPrec sp sa d (Freer c) = showsUnaryWith (liftShowsPrec2 sp sa (liftShowsPrec sp sa) (liftShowList sp sa)) "Freer" d c

instance (Show1 f, Show a) => Show (Freer f a) where
  showsPrec = liftShowsPrec showsPrec showList

instance Eq1 f => Eq1 (Freer f) where
  liftEq eqA = go where go (Freer f1) (Freer f2) = liftEq2 eqA go f1 f2

instance (Eq1 f, Eq a) => Eq (Freer f a) where
  (==) = liftEq (==)

instance Listable1 f => Listable1 (Freer f) where
  liftTiers t1 = go
    where go = liftCons1 (liftTiers2 t1 go) Freer

instance (Listable a, Listable1 f) => Listable (Freer f a) where
  tiers = liftTiers tiers
