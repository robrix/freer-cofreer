{-# LANGUAGE FlexibleInstances, GADTs, MultiParamTypeClasses, RankNTypes, TypeFamilies #-}
module Control.Comonad.Cofree.Cofreer
( Cofreer(..)
, cowrap
, coiter
, unfold
, hoistCofreer
, extract
, unwrap
) where

import Control.Arrow ((&&&))
import Control.Comonad
import Control.Comonad.Cofree.Class
import qualified Control.Comonad.Trans.Cofree.Cofreer as T
import Data.Bifunctor
import Data.Functor.Classes
import Data.Functor.Foldable hiding (unfold)
import Data.Functor.Listable

data Cofreer f a where
  Cofree :: a -> f x -> (x -> Cofreer f a) -> Cofreer f a

infixr 5 `cowrap`
cowrap :: a -> f (Cofreer f a) -> Cofreer f a
cowrap a r = Cofree a r id

coiter :: Functor f => (b -> f b) -> b -> Cofreer f b
coiter f = unfold (id &&& f)

unfold :: Functor f => (b -> (a, f b)) -> b -> Cofreer f a
unfold f c = let (x, d) = f c in Cofree x d (unfold f)


hoistCofreer :: (forall a. f a -> g a) -> Cofreer f b -> Cofreer g b
hoistCofreer f = go
  where go (Cofree a r t) = Cofree a (f r) (go . t)


-- Instances

instance Functor (Cofreer f) where
  fmap f = go
    where go (Cofree a r t) = Cofree (f a) r (go . t)

instance Comonad (Cofreer f) where
  extract (Cofree a _ _) = a
  extend f c@(Cofree _ r t) = Cofree (f c) r (extend f . t)

instance Functor f => ComonadCofree f (Cofreer f) where
  unwrap (Cofree _ r t) = fmap t r


instance Foldable f => Foldable (Cofreer f) where
  foldMap f = go
    where go (Cofree a r t) = mappend (f a) (foldMap (go . t) r)

instance Traversable f => Traversable (Cofreer f) where
  traverse f = go
    where go (Cofree a r t) = cowrap <$> f a <*> traverse (go . t) r


type instance Base (Cofreer f a) = T.CofreerF f a

instance Recursive (Cofreer f a) where
  project (Cofree a r t) = T.Cofree a r t

instance Corecursive (Cofreer f a) where
  embed (T.Cofree a r t) = Cofree a r t


instance Show1 f => Show1 (Cofreer f) where
  liftShowsPrec sp sl = go
    where go d (Cofree a r t) = showsTernaryWith sp (liftShowsPrec (\ i -> go i . t) (liftShowList sp sl . fmap t)) (const showString) "Cofree" d a r "_"
          showsTernaryWith :: (Int -> a -> ShowS) -> (Int -> b -> ShowS) -> (Int -> c -> ShowS) -> String -> Int -> a -> b -> c -> ShowS
          showsTernaryWith sp1 sp2 sp3 name d x y z = showParen (d > 10) $ showString name . showChar ' ' . sp1 11 x . showChar ' ' . sp2 11 y . showChar ' ' . sp3 11 z

instance (Show1 f, Show a) => Show (Cofreer f a) where
  showsPrec = liftShowsPrec showsPrec showList


instance Eq1 f => Eq1 (Cofreer f) where
  liftEq eqA = go
    where go (Cofree a1 r1 t1) (Cofree a2 r2 t2) = eqA a1 a2 && liftEq (\ x1 x2 -> go (t1 x1) (t2 x2)) r1 r2

instance (Eq1 f, Eq a) => Eq (Cofreer f a) where
  (==) = liftEq (==)


instance Listable1 f => Listable1 (Cofreer f) where
  liftTiers t1 = go
    where go = liftCons2 t1 (liftTiers go) (\ r t -> Cofree r t id)

instance (Listable a, Listable1 f) => Listable (Cofreer f a) where
  tiers = liftTiers tiers
