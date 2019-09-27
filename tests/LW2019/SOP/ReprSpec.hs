{-# LANGUAGE StandaloneDeriving #-}
module LW2019.SOP.ReprSpec (spec) where

import LW2019.Types.Regular
import LW2019.Types.Instances.Arbitrary
import LW2019.Generics.SOP.Repr

import Generics.SOP

import Test.QuickCheck
import Test.Hspec

deriving instance Eq QualName

spec :: Spec
spec = describe "SOP" $ describe "Repr" $ do
  describe "Generic Conversions" $ do
    it "to-from is id" $ property $ 
      \x -> to (from (x :: QualName)) == x
