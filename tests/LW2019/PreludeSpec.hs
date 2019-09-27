module LW2019.PreludeSpec (spec) where

import LW2019.Prelude

import Test.QuickCheck
import Test.Hspec

spec :: Spec
spec = describe "Prelude" $ do
  describe "Generic Conversions" $ do
    it "to-from is id" $ property $ and $
      [ to (from t1) == t1
      , to (from t2) == t2
      , deep_to (deep_from t1) == t1
      , deep_to (deep_from t2) == t2
      ]
