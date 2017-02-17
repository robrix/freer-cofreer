{-# LANGUAGE GADTs #-}
module Control.Comonad.Trans.Cofree.Cofreer where

import Data.Bifunctor
import Data.Functor.Classes
import Data.Functor.Listable

data CofreerF f a b where
  Cofree :: a -> (x -> b) -> f x -> CofreerF f a b

headF :: CofreerF f a b -> a
headF (Cofree a _ _) = a

tailF :: Functor f => CofreerF f a b -> f b
tailF (Cofree _ t r) = t <$> r


-- Instances

instance Bifunctor (CofreerF f) where
  bimap f g (Cofree a t r) = Cofree (f a) (g . t) r

instance Functor (CofreerF f a) where
  fmap = second


instance Foldable f => Foldable (CofreerF f a) where
  foldMap f (Cofree _ t r) = foldMap (f . t) r

instance Traversable f => Traversable (CofreerF f a) where
  traverse f (Cofree a t r) = Cofree a id <$> traverse (f . t) r


instance Show1 f => Show2 (CofreerF f) where
  liftShowsPrec2 sp1 _ sp2 sa2 d (Cofree a t r) = showsTernaryWith sp1 (const showString) (liftShowsPrec (\ i -> sp2 i . t) (sa2 . fmap t)) "Cofree" d a "id" r
    where showsTernaryWith :: (Int -> a -> ShowS) -> (Int -> b -> ShowS) -> (Int -> c -> ShowS) -> String -> Int -> a -> b -> c -> ShowS
          showsTernaryWith sp1 sp2 sp3 name d x y z = showParen (d > 10) $ showString name . showChar ' ' . sp1 11 x . showChar ' ' . sp2 11 y . showChar ' ' . sp3 11 z

instance (Show1 f, Show a) => Show1 (CofreerF f a) where
  liftShowsPrec = liftShowsPrec2 showsPrec showList

instance (Show1 f, Show a, Show b) => Show (CofreerF f a b) where
  showsPrec = liftShowsPrec showsPrec showList


instance Eq1 f => Eq2 (CofreerF f) where
  liftEq2 eqA eqB (Cofree a1 t1 r1) (Cofree a2 t2 r2) = eqA a1 a2 && liftEq (\ x1 x2 -> eqB (t1 x1) (t2 x2)) r1 r2

instance (Eq1 f, Eq a) => Eq1 (CofreerF f a) where
  liftEq = liftEq2 (==)

instance (Eq1 f, Eq a, Eq b) => Eq (CofreerF f a b) where
  (==) = liftEq (==)


instance Listable1 f => Listable2 (CofreerF f) where
  liftTiers2 t1 t2 = liftCons2 t1 (liftTiers t2) (`Cofree` id)

instance (Listable a, Listable1 f) => Listable1 (CofreerF f a) where
  liftTiers = liftTiers2 tiers

instance (Listable a, Listable b, Listable1 f) => Listable (CofreerF f a b) where
  tiers = liftTiers tiers
