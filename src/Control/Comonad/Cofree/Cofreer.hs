{-# LANGUAGE FlexibleInstances, GADTs, MultiParamTypeClasses, RankNTypes, TypeFamilies #-}
module Control.Comonad.Cofree.Cofreer
( CofreerF(..)
, Cofreer(..)
, headF
, tailF
, cowrap
, coiter
, unfold
, hoistCofreer
, hoistCofreerF
, extract
, unwrap
) where

import Control.Arrow ((&&&))
import Control.Comonad
import Control.Comonad.Cofree.Class
import Control.Comonad.Trans.Cofree.Cofreer
import Data.Bifunctor
import Data.Functor.Classes
import Data.Functor.Foldable hiding (unfold)
import Data.Functor.Listable

newtype Cofreer f a = Cofreer { runCofreer :: CofreerF f a (Cofreer f a) }

infixr 5 `cowrap`
cowrap :: a -> f (Cofreer f a) -> Cofreer f a
cowrap a r = Cofreer (Cofree a r id)

coiter :: Functor f => (b -> f b) -> b -> Cofreer f b
coiter f = unfold (id &&& f)

unfold :: Functor f => (b -> (a, f b)) -> b -> Cofreer f a
unfold f c = let (x, d) = f c in Cofreer (Cofree x d (unfold f))


hoistCofreer :: (forall a. f a -> g a) -> Cofreer f b -> Cofreer g b
hoistCofreer f = go
  where go = Cofreer . fmap go . hoistCofreerF f . runCofreer


-- Instances

instance Functor (Cofreer f) where
  fmap f = Cofreer . bimap f (fmap f) . runCofreer

instance Comonad (Cofreer f) where
  extract (Cofreer (Cofree a _ _)) = a
  extend f c@(Cofreer (Cofree _ r t)) = Cofreer (Cofree (f c) r (extend f . t))

instance Functor f => ComonadCofree f (Cofreer f) where
  unwrap = tailF . runCofreer


instance Foldable f => Foldable (Cofreer f) where
  foldMap f (Cofreer c) = mappend (f (headF c)) (foldMap (foldMap f) c)

instance Traversable f => Traversable (Cofreer f) where
  traverse f (Cofreer (Cofree a r t)) = cowrap <$> f a <*> traverse (traverse f . t) r


type instance Base (Cofreer f a) = CofreerF f a

instance Recursive (Cofreer f a) where project = runCofreer
instance Corecursive (Cofreer f a) where embed = Cofreer


instance Show1 f => Show1 (Cofreer f) where
  liftShowsPrec sp sa d (Cofreer c) = showsUnaryWith (liftShowsPrec2 sp sa (liftShowsPrec sp sa) (liftShowList sp sa)) "Cofreer" d c

instance (Show1 f, Show a) => Show (Cofreer f a) where
  showsPrec = liftShowsPrec showsPrec showList


instance Eq1 f => Eq1 (Cofreer f) where
  liftEq eqA = go where go (Cofreer f1) (Cofreer f2) = liftEq2 eqA go f1 f2

instance (Eq1 f, Eq a) => Eq (Cofreer f a) where
  (==) = liftEq (==)


instance Listable1 f => Listable1 (Cofreer f) where
  liftTiers t1 = go
    where go = liftCons1 (liftTiers2 t1 go) Cofreer

instance (Listable a, Listable1 f) => Listable (Cofreer f a) where
  tiers = liftTiers tiers
