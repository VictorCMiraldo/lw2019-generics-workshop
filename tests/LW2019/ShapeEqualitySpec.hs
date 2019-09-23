{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ConstraintKinds    #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE KindSignatures     #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE PolyKinds          #-}
{-# LANGUAGE DataKinds          #-}
module LW2019.ShapeEqualitySpec (spec) where

import Data.Proxy

import LW2019.Types.Regular
import LW2019.Types.MutuallyRecursive
import LW2019.Types.Instances.Arbitrary

import qualified LW2019.Generics.GHC.ShapeEquality    as GHC
import qualified LW2019.Generics.SOP.ShapeEquality    as SOP
import qualified LW2019.Generics.MRSOP.ShapeEquality  as MRSOP
import           LW2019.Generics.MRSOP.Repr

import Generics.MRSOP.Base

import GHC.Generics

import Test.QuickCheck
import Test.Hspec

spec :: Spec
spec = describe "ShapeEquality" $ do
  describe "Generics.GHC.ShapeEquality" $ do
    it "absorbs map" $ property $
      \x -> GHC.shapeEq (Proxy :: Proxy (Tree12 Int))
                        (x :: Tree12 Int)
                        (fmap (+10) x)

    it "fails for select cases" $ property $ and $ map not
        [ GHC.shapeEq (Proxy :: Proxy (Tree12 Int))
            Leaf (Node1 (3 :: Int) Leaf)
        , GHC.shapeEq (Proxy :: Proxy QualName)
            (Base "test") (Qual "test" (Base "test"))
        ]

  describe "Generics.SOP.ShapeEquality" $ do
    it "absorbs map" $ property $
      \x -> SOP.shapeEq (x :: Tree12 Int) (fmap (+10) x)

    it "fails for select cases" $ property $ and $ map not
        [ SOP.shapeEq Leaf (Node1 (3 :: Int) Leaf)
        , SOP.shapeEq (Base "test") (Qual "test" (Base "test"))
        ]


  describe "Generics.MRSOP.ShapeEquality" $ do
    it "absorbs map" $ property $
      \x -> MRSOP.shapeEq (into @FamTree12Int @_ @Z x)
                          (into $ fmap (+10) x)

    it "fails for select cases" $ property $ and $ map not
        [ MRSOP.shapeEq (into @FamTree12Int @_ @Z Leaf)
                        (into $ Node1 (3 :: Int) Leaf)
        , MRSOP.shapeEq (into @FamQualName_ @_ @Z $ Base "test")
                        (into $ Qual "test" (Base "test"))
        ]

