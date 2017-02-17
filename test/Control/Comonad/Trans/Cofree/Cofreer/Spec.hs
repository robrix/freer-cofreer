module Control.Comonad.Trans.Cofree.Cofreer.Spec where

import Control.Comonad.Trans.Cofree.Cofreer
import Test.Hspec
import Test.Hspec.LeanCheck

spec :: Spec
spec = do
  describe "CofreerF" $ do
    describe "Eq" $ do
      prop "is reflexive" $
        \ a -> a `shouldBe` (a :: CofreerF Maybe Int Int)

      prop "is commutative" $
        \ a b -> a == b `shouldBe` b == (a :: CofreerF Maybe Int Int)
