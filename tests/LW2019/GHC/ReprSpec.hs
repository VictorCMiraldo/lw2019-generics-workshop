{-# LANGUAGE StandaloneDeriving #-}
module LW2019.GHC.ReprSpec (spec) where

import LW2019.Types.Regular
import LW2019.Types.Instances.Arbitrary
import LW2019.Generics.GHC.Repr

import GHC.Generics

import Test.QuickCheck
import Test.Hspec

deriving instance Eq QualName

spec :: Spec
spec = describe "GHC" $ describe "Repr" $ do
  describe "Generic Conversions" $ do
    it "to-from is id" $ property $ 
      \x -> to (from (x :: QualName)) == x

    it "keeps data" $ property $
      from (Qual "a" (Base "b"))
        == R1 (K1 "a" :*: K1 (Base "b"))
