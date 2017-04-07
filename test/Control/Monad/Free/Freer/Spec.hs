{-# LANGUAGE GADTs #-}
module Control.Monad.Free.Freer.Spec where

import Control.Monad.Free.Freer
import Data.Functor.Classes
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
          pure ()) `shouldBe` "Set 0\nTest 0\nSet 1\nTest 1\nReturn ()"


data Instr a where
  Set :: Int -> Instr ()
  Test :: Int -> Instr Bool

set :: Int -> Freer Instr ()
set i = Set i `Then` return

test :: Int -> Freer Instr Bool
test i = Test i `Then` return

instance Show1 Instr where
  liftShowsPrec sp _ d i = case i of
    Set i -> showsUnaryWith showsPrec "Set" d i . showChar '\n' . sp d ()
    Test i -> showsUnaryWith showsPrec "Test" d i . showChar '\n' . sp d False
