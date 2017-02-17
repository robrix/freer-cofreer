module Control.Comonad.Cofree.Cofreer.Spec where

import Control.Comonad.Cofree.Cofreer
import Test.Hspec
import Test.Hspec.LeanCheck

spec :: Spec
spec = do
  describe "Cofreer" $ do
    describe "Eq" $ do
      prop "is reflexive" $
        \ a -> a `shouldBe` (a :: Cofreer Maybe Int)

      prop "is commutative" $
        \ a b -> a == b `shouldBe` b == (a :: Cofreer Maybe Int)
