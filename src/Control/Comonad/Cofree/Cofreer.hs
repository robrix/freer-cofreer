{-# LANGUAGE FlexibleInstances, GADTs, MultiParamTypeClasses, RankNTypes, TypeFamilies #-}
module Control.Comonad.Cofree.Cofreer
( Cofreer(..)
, cowrap
, hoistCofreer
, CofreerF(..)
, headF
, tailF
, coiter
, unfold
, extract
, unwrap
) where

import Control.Arrow ((&&&))
import Control.Comonad
import Control.Comonad.Cofree.Class
import Data.Bifunctor
import Data.Functor.Classes
import Data.Functor.Foldable hiding (unfold)

data Cofreer f a where
  Cofree :: a -> f x -> (x -> Cofreer f a) -> Cofreer f a

infixr 5 `cowrap`

cowrap :: a -> f (Cofreer f a) -> Cofreer f a
cowrap a r = Cofree a r id
{-# INLINE cowrap #-}

hoistCofreer :: (forall a. f a -> g a) -> Cofreer f b -> Cofreer g b
hoistCofreer f = go
  where go (Cofree a r t) = Cofree a (f r) (go . t)


data CofreerF f a b where
  CofreeF :: a -> f x -> (x -> b) -> CofreerF f a b

headF :: CofreerF f a b -> a
headF (CofreeF a _ _) = a
{-# INLINE headF #-}

tailF :: Functor f => CofreerF f a b -> f b
tailF (CofreeF _ r t) = t <$> r
{-# INLINE tailF #-}


hoistCofreerF :: (forall a. f a -> g a) -> CofreerF f b c -> CofreerF g b c
hoistCofreerF f (CofreeF a r t) = CofreeF a (f r) t


coiter :: Functor f => (b -> f b) -> b -> Cofreer f b
coiter f = unfold (id &&& f)

unfold :: Functor f => (b -> (a, f b)) -> b -> Cofreer f a
unfold f c = let (x, d) = f c in Cofree x d (unfold f)


-- Instances

instance Functor (Cofreer f) where
  fmap f = go
    where go (Cofree a r t) = Cofree (f a) r (go . t)
  {-# INLINE fmap #-}

instance Comonad (Cofreer f) where
  extract (Cofree a _ _) = a
  {-# INLINE extract #-}
  extend f c@(Cofree _ r t) = Cofree (f c) r (extend f . t)

instance Functor f => ComonadCofree f (Cofreer f) where
  unwrap (Cofree _ r t) = fmap t r
  {-# INLINE unwrap #-}


instance Foldable f => Foldable (Cofreer f) where
  foldMap f = go
    where go (Cofree a r t) = mappend (f a) (foldMap (go . t) r)
  {-# INLINE foldMap #-}

instance Traversable f => Traversable (Cofreer f) where
  traverse f = go
    where go (Cofree a r t) = cowrap <$> f a <*> traverse (go . t) r
  {-# INLINE traverse #-}


instance Show1 f => Show1 (Cofreer f) where
  liftShowsPrec sp sl = go
    where go d (Cofree a r t) = showsTernaryWith sp (liftShowsPrec ((. t) . go) (liftShowList sp sl . fmap t)) (const showString) "Cofree" d a r "_"
          showsTernaryWith :: (Int -> a -> ShowS) -> (Int -> b -> ShowS) -> (Int -> c -> ShowS) -> String -> Int -> a -> b -> c -> ShowS
          showsTernaryWith sp1 sp2 sp3 name d x y z = showParen (d > 10) $ showString name . showChar ' ' . sp1 11 x . showChar ' ' . sp2 11 y . showChar ' ' . sp3 11 z

instance (Show1 f, Show a) => Show (Cofreer f a) where
  showsPrec = liftShowsPrec showsPrec showList


instance Eq1 f => Eq1 (Cofreer f) where
  liftEq eqA = go
    where go (Cofree a1 r1 t1) (Cofree a2 r2 t2) = eqA a1 a2 && liftEq (\ x1 x2 -> go (t1 x1) (t2 x2)) r1 r2

instance (Eq1 f, Eq a) => Eq (Cofreer f a) where
  (==) = liftEq (==)


instance Bifunctor (CofreerF f) where
  bimap f g (CofreeF a r t) = CofreeF (f a) r (g . t)
  {-# INLINE bimap #-}

instance Functor (CofreerF f a) where
  fmap f (CofreeF a r t) = CofreeF a r (f . t)
  {-# INLINE fmap #-}


instance Foldable f => Foldable (CofreerF f a) where
  foldMap f (CofreeF _ r t) = foldMap (f . t) r
  {-# INLINE foldMap #-}

instance Traversable f => Traversable (CofreerF f a) where
  traverse f (CofreeF a r t) = flip (CofreeF a) id <$> traverse (f . t) r
  {-# INLINE traverse #-}


instance Show1 f => Show2 (CofreerF f) where
  liftShowsPrec2 sp1 _ sp2 sa2 d (CofreeF a r t) = showsTernaryWith sp1 (liftShowsPrec (\ i -> sp2 i . t) (sa2 . fmap t)) (const showString) "CofreeF" d a r "_"
    where showsTernaryWith :: (Int -> a -> ShowS) -> (Int -> b -> ShowS) -> (Int -> c -> ShowS) -> String -> Int -> a -> b -> c -> ShowS
          showsTernaryWith sp1 sp2 sp3 name d x y z = showParen (d > 10) $ showString name . showChar ' ' . sp1 11 x . showChar ' ' . sp2 11 y . showChar ' ' . sp3 11 z

instance (Show1 f, Show a) => Show1 (CofreerF f a) where
  liftShowsPrec = liftShowsPrec2 showsPrec showList

instance (Show1 f, Show a, Show b) => Show (CofreerF f a b) where
  showsPrec = liftShowsPrec showsPrec showList


instance Eq1 f => Eq2 (CofreerF f) where
  liftEq2 eqA eqB (CofreeF a1 r1 t1) (CofreeF a2 r2 t2) = eqA a1 a2 && liftEq (\ x1 x2 -> eqB (t1 x1) (t2 x2)) r1 r2

instance (Eq1 f, Eq a) => Eq1 (CofreerF f a) where
  liftEq = liftEq2 (==)

instance (Eq1 f, Eq a, Eq b) => Eq (CofreerF f a b) where
  (==) = liftEq (==)


type instance Base (Cofreer f a) = CofreerF f a

instance Recursive (Cofreer f a) where
  project (Cofree a r t) = CofreeF a r t
  {-# INLINE project #-}

instance Corecursive (Cofreer f a) where
  embed (CofreeF a r t) = Cofree a r t
  {-# INLINE embed #-}
