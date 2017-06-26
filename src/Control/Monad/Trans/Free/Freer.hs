{-# LANGUAGE GADTs, RankNTypes #-}
module Control.Monad.Trans.Free.Freer where

import Data.Bifunctor
import Data.Functor.Classes
import Data.Functor.Listable

data FreerF f a b where
  Return :: a -> FreerF f a b
  Then :: f x -> (x -> b) -> FreerF f a b


liftFreerF :: f b -> FreerF f a b
liftFreerF = flip Then id

hoistFreerF :: (forall a. f a -> g a) -> FreerF f b c -> FreerF g b c
hoistFreerF f r = case r of
  Return a -> Return a
  Then r t -> Then (f r) t


-- Instances

instance Bifunctor (FreerF f) where
  bimap f g r = case r of
    Return a -> Return (f a)
    Then r t -> Then r (g . t)
  {-# INLINE bimap #-}

instance Functor (FreerF f a) where
  fmap = second
  {-# INLINE fmap #-}


instance Foldable f => Foldable (FreerF f a) where
  foldMap f g = case g of
    Return _ -> mempty
    Then r t -> foldMap (f . t) r
  {-# INLINE foldMap #-}

instance Traversable f => Traversable (FreerF f a) where
  traverse f g = case g of
    Return a -> pure (Return a)
    Then r t -> flip Then id <$> traverse (f . t) r
  {-# INLINE traverse #-}


instance Eq1 f => Eq2 (FreerF f) where
  liftEq2 eqA eqB f1 f2 = case (f1, f2) of
    (Return a1, Return a2) -> eqA a1 a2
    (Then r1 t1, Then r2 t2) -> liftEq (\ x1 x2 -> eqB (t1 x1) (t2 x2)) r1 r2
    _ -> False

instance (Eq1 f, Eq a) => Eq1 (FreerF f a) where
  liftEq = liftEq2 (==)

instance (Eq1 f, Eq a, Eq b) => Eq (FreerF f a b) where
  (==) = liftEq (==)


instance Show1 f => Show2 (FreerF f) where
  liftShowsPrec2 sp1 _ sp2 sa2 d f = case f of
    Return a -> showsUnaryWith sp1 "Return" d a
    Then r t -> showsBinaryWith (liftShowsPrec (\ i -> sp2 i . t) (sa2 . fmap t)) (const showString) "Free" d r "_"

instance (Show1 f, Show a) => Show1 (FreerF f a) where
  liftShowsPrec = liftShowsPrec2 showsPrec showList

instance (Show1 f, Show a, Show b) => Show (FreerF f a b) where
  showsPrec = liftShowsPrec showsPrec showList


instance Listable1 f => Listable2 (FreerF f) where
  liftTiers2 t1 t2 = liftCons1 t1 Return \/ liftCons1 (liftTiers t2) liftFreerF

instance (Listable a, Listable1 f) => Listable1 (FreerF f a) where
  liftTiers = liftTiers2 tiers

instance (Listable a, Listable b, Listable1 f) => Listable (FreerF f a b) where
  tiers = liftTiers tiers
