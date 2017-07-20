module Control.Comonad.Cofree.Cofreer.Spec where

import Control.Comonad.Cofree.Cofreer
import Data.Functor.Listable
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


instance Listable1 f => Listable1 (Cofreer f) where
  liftTiers t1 = go
    where go = liftCons2 t1 (liftTiers go) (\ r t -> Cofree r t id)

instance (Listable a, Listable1 f) => Listable (Cofreer f a) where
  tiers = liftTiers tiers


instance Listable1 f => Listable2 (CofreerF f) where
  liftTiers2 t1 t2 = liftCons2 t1 (liftTiers t2) (\ a r -> CofreeF a r id)

instance (Listable a, Listable1 f) => Listable1 (CofreerF f a) where
  liftTiers = liftTiers2 tiers

instance (Listable a, Listable b, Listable1 f) => Listable (CofreerF f a b) where
  tiers = liftTiers tiers
