{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds        #-}
module LW2019.HeightSpec (spec) where

import LW2019.Types.Regular
import LW2019.Types.MutuallyRecursive
import LW2019.Types.Instances.Arbitrary

import qualified LW2019.Generics.GHC.Height    as GHC
import qualified LW2019.Generics.SOP.Height    as SOP
import qualified LW2019.Generics.MRSOP.Height  as MRSOP
import qualified LW2019.Generics.MRSOP.Repr    as MRSOP

import Generics.MRSOP.Base

import Test.QuickCheck
import Test.Hspec

sampleTree0 :: Tree12 ()
sampleTree0 = Node2 () (Node1 () Leaf)
                      (Node1 () (Node1 () Leaf))

sampleTree1 :: Tree12 ()
sampleTree1 = Node2 () sampleTree0
                       (Node1 () sampleTree0)

spec :: Spec
spec = describe "Height" $ do
  describe "Generics.GHC.Height" $ do
    it "Is correct for select cases" $ property $ and $
      [ GHC.heightTree12 sampleTree0  == 3
      , GHC.heightTree12 sampleTree1  == 5
      , GHC.heightQualName (Base "a") == 0
      ]

  describe "Generics.SOP.Height" $ do
    it "is equal to GHC.Height" $ property $
      \x -> GHC.heightTree12 (x :: Tree12 Int) == SOP.gheight x

  describe "Generics.MRSOP.Height" $ do
    it "gheight0 is equal to GHC.Height" $ property $
      \x -> GHC.heightTree12 (x :: Tree12 Int)
            == MRSOP.gheight0 (into @MRSOP.FamTree12Int x)
            
    it "gheight1 is equal to GHC.Height" $ property $
      \x -> GHC.heightTree12 (x :: Tree12 Int)
            == MRSOP.gheight1 (into @MRSOP.FamTree12Int x)
