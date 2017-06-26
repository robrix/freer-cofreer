{-# LANGUAGE GADTs, RankNTypes #-}
module Control.Comonad.Trans.Cofree.Cofreer where

import Data.Bifunctor
import Data.Functor.Classes
import Data.Functor.Listable

data CofreerF f a b where
  Cofree :: a -> f x -> (x -> b) -> CofreerF f a b

headF :: CofreerF f a b -> a
headF (Cofree a _ _) = a
{-# INLINE headF #-}

tailF :: Functor f => CofreerF f a b -> f b
tailF (Cofree _ r t) = t <$> r
{-# INLINE tailF #-}


hoistCofreerF :: (forall a. f a -> g a) -> CofreerF f b c -> CofreerF g b c
hoistCofreerF f (Cofree a r t) = Cofree a (f r) t


-- Instances

instance Bifunctor (CofreerF f) where
  bimap f g (Cofree a r t) = Cofree (f a) r (g . t)
  {-# INLINE bimap #-}

instance Functor (CofreerF f a) where
  fmap = second
  {-# INLINE fmap #-}


instance Foldable f => Foldable (CofreerF f a) where
  foldMap f (Cofree _ r t) = foldMap (f . t) r
  {-# INLINE foldMap #-}

instance Traversable f => Traversable (CofreerF f a) where
  traverse f (Cofree a r t) = flip (Cofree a) id <$> traverse (f . t) r
  {-# INLINE traverse #-}


instance Show1 f => Show2 (CofreerF f) where
  liftShowsPrec2 sp1 _ sp2 sa2 d (Cofree a r t) = showsTernaryWith sp1 (liftShowsPrec (\ i -> sp2 i . t) (sa2 . fmap t)) (const showString) "Cofree" d a r "_"
    where showsTernaryWith :: (Int -> a -> ShowS) -> (Int -> b -> ShowS) -> (Int -> c -> ShowS) -> String -> Int -> a -> b -> c -> ShowS
          showsTernaryWith sp1 sp2 sp3 name d x y z = showParen (d > 10) $ showString name . showChar ' ' . sp1 11 x . showChar ' ' . sp2 11 y . showChar ' ' . sp3 11 z

instance (Show1 f, Show a) => Show1 (CofreerF f a) where
  liftShowsPrec = liftShowsPrec2 showsPrec showList

instance (Show1 f, Show a, Show b) => Show (CofreerF f a b) where
  showsPrec = liftShowsPrec showsPrec showList


instance Eq1 f => Eq2 (CofreerF f) where
  liftEq2 eqA eqB (Cofree a1 r1 t1) (Cofree a2 r2 t2) = eqA a1 a2 && liftEq (\ x1 x2 -> eqB (t1 x1) (t2 x2)) r1 r2

instance (Eq1 f, Eq a) => Eq1 (CofreerF f a) where
  liftEq = liftEq2 (==)

instance (Eq1 f, Eq a, Eq b) => Eq (CofreerF f a b) where
  (==) = liftEq (==)


instance Listable1 f => Listable2 (CofreerF f) where
  liftTiers2 t1 t2 = liftCons2 t1 (liftTiers t2) (\ a r -> Cofree a r id)

instance (Listable a, Listable1 f) => Listable1 (CofreerF f a) where
  liftTiers = liftTiers2 tiers

instance (Listable a, Listable b, Listable1 f) => Listable (CofreerF f a b) where
  tiers = liftTiers tiers
