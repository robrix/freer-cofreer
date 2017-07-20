{-# LANGUAGE GADTs #-}
module Control.Monad.Free.Freer.Spec where

import Control.Monad.Free.Freer
import Data.Functor.Classes
import Data.Functor.Listable
import Test.Hspec
import Test.Hspec.LeanCheck

spec :: Spec
spec = do
  describe "Freer" $ do
    describe "Eq" $ do
      prop "is reflexive" $
        \ a -> a `shouldBe` (a :: Freer Maybe Int)

      prop "is commutative" $
        \ a b -> a == b `shouldBe` b == (a :: Freer Maybe Int)

    describe "Show" $ do
      it "shows complete programs" $
        show (do
          set 0
          test 0
          set 1
          test 1
          pure ()) `shouldBe` "Then (Set 0) (Then (Test 0) (Then (Set 1) (Then (Test 1) (Return ()) _) _) _) _"


data Instr a where
  Set :: Int -> Instr ()
  Test :: Int -> Instr Bool

set :: Int -> Freer Instr ()
set i = Set i `Then` return

test :: Int -> Freer Instr Bool
test i = Test i `Then` return

instance Show1 Instr where
  liftShowsPrec sp _ d i = case i of
    Set i -> showsUnaryWith showsPrec "Set" d i . showChar ' ' . sp d ()
    Test i -> showsUnaryWith showsPrec "Test" d i . showChar ' ' . sp d False


instance Listable1 f => Listable1 (Freer f) where
  liftTiers t1 = go where go = liftCons1 t1 Return \/ liftCons1 (liftTiers go) wrap

instance (Listable a, Listable1 f) => Listable (Freer f a) where
  tiers = liftTiers tiers


instance Listable1 f => Listable2 (FreerF f) where
  liftTiers2 t1 t2 = liftCons1 t1 ReturnF \/ liftCons1 (liftTiers t2) liftFreerF

instance (Listable a, Listable1 f) => Listable1 (FreerF f a) where
  liftTiers = liftTiers2 tiers

instance (Listable a, Listable b, Listable1 f) => Listable (FreerF f a b) where
  tiers = liftTiers tiers
