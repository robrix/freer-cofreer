{-# LANGUAGE FlexibleInstances, GADTs, TypeFamilies #-}
module Test.Hspec.LeanCheck
( prop
, forAll
) where

import Control.Exception
import Data.Bifunctor (first)
import Data.Maybe (listToMaybe)
import Data.String (String)
import GHC.Stack
import Test.Hspec
import Test.Hspec.Core.Spec
import qualified Test.HUnit.Lang as HUnit
import Test.LeanCheck.Core

data Property where
  Property :: IOTestable prop => prop -> Property

-- | Perform an enumerative test of a property using LeanCheck.
--
--   'prop' will typically be a function of one or more 'Listable' arguments, returning either 'Bool' or 'IO ()' (in the latter case, typically via 'shouldBe' and friends). For example:
--
-- > describe "+" $ do
-- >   prop "associativity" $
-- >     \ a b c -> a + (b + c) `shouldBe` (a + b :: Int) + c
prop :: (HasCallStack, IOTestable prop) => String -> prop -> Spec
prop s = it s . Property

data ForAll a where
  ForAll :: IOTestable prop => [[a]] -> (a -> prop) -> ForAll a

-- | Test a property given by an explicit list of tiers rather than a 'Listable' instance. This can be used to e.g. filter input values for which the property does not hold.
--
-- > describe "mean" $ do
-- >   prop "≥ the minimum" . forAll (not . null `filterT` tiers) $
-- >     \ list -> (mean list :: Int) `shouldSatisfy` (>= min list)
forAll :: IOTestable prop => [[a]] -> (a -> prop) -> ForAll a
forAll = ForAll

instance Example Property where
  type Arg Property = ()
  evaluateExample (Property prop) (Params _ bound) _ _ =
    (toResult <$> iocounterExample bound prop)
      `catch` \ e -> pure $! failureToResult (e :: HUnit.HUnitFailure)
    where toResult = maybe Success (Fail Nothing . concat)
          failureToResult (HUnit.HUnitFailure mLoc err) = Fail (toLocation <$> mLoc) err
          toLocation loc = Location (HUnit.locationFile loc) (HUnit.locationLine loc) (HUnit.locationColumn loc) ExactLocation


class IOTestable t where
  -- 'resultiers', lifted into 'IO'.
  ioResultTiers :: t -> [[IO ([String], Bool)]]

instance IOTestable (IO ()) where
  ioResultTiers action = [[ action >> pure ([], True) ]]

instance (IOTestable b, Show a, Listable a) => IOTestable (a -> b) where
  ioResultTiers p = ioconcatMapT resultiersFor tiers
    where resultiersFor x = fmap (fmap (first (showsPrec 11 x "":))) <$> ioResultTiers (p x)

instance IOTestable Bool where
  ioResultTiers p = [[ pure ([], p) ]]

instance IOTestable (ForAll a) where
  ioResultTiers (ForAll tiers property) = concatMapT (ioResultTiers . property) tiers


-- | 'concatMapT', lifted into 'IO'.
ioconcatMapT :: (a -> [[IO b]]) -> [[a]] -> [[IO b]]
ioconcatMapT f = (>>= (>>= f))

-- | 'counterExamples', lifted into 'IO'.
iocounterExamples :: IOTestable a => Int -> a -> IO [[String]]
iocounterExamples n = fmap (fmap fst . filter (not . snd)) . sequenceA . take n . concat . ioResultTiers

-- | 'counterExample', lifted into 'IO'.
iocounterExample :: IOTestable a => Int -> a -> IO (Maybe [String])
iocounterExample n = fmap listToMaybe . iocounterExamples n
