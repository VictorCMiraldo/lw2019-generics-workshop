{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE KindSignatures   #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GADTs            #-}
module LW2019.GHC.EqualitySpec (spec) where

import Data.Proxy

import LW2019.Types.Regular
import LW2019.Types.MutuallyRecursive
import LW2019.Types.Instances.Arbitrary

import qualified LW2019.Generics.GHC.Equality    as GHC
import qualified LW2019.Generics.SOP.Equality    as SOP
import qualified LW2019.Generics.MRSOP.Equality  as MRSOP

import Test.QuickCheck
import Test.Hspec

spec :: Spec
spec = do

  describe "Generics.GHC.Equality" $ do
    it "fails for select cases" $ property $ and $ map not
      [ GHC.eq Leaf                  (Node1 (3 :: Int) Leaf)
      , GHC.eq (Base "test")         (Qual "test" (Base "test"))
      , GHC.eq (BookInfo "a" "b" 1)  (BookInfo "a" "b" 2)
      , GHC.eq (BookInfo "a'" "b" 1) (BookInfo "a" "b" 1)
      ]
    it "for BookInfo" $ property $ \x -> GHC.eq @BookInfo x x
    it "for QualName" $ property $ \x -> GHC.eq @QualName x x
    it "for Tree12"   $ property $ \x -> GHC.eq @(Tree12 Float) x x


  describe "Generics.SOP.Equality" $ do
    it "fails for select cases" $ property $ and $ map not
      [ SOP.geq Leaf                  (Node1 (3 :: Int) Leaf)
      , SOP.geq (Base "test")         (Qual "test" (Base "test"))
      , SOP.geq (BookInfo "a" "b" 1)  (BookInfo "a" "b" 2)
      , SOP.geq (BookInfo "a'" "b" 1) (BookInfo "a" "b" 1)
      ]
    it "retrofits GHC.Equality" $ property $
      \x y -> GHC.eq @(Tree12 Float) x y == SOP.geq x y

  describe "Generics.MRSOP.Equality" $ do
    it "retrofits GHC.Equality" $ property $
      \x y -> MRSOP.geq (into x) (into y) == GHC.eq x y
{-
    it "fails for select cases" $ property $ and $ map not
      [ MRSOP.geq (into Leaf)                  (into (Node1 (3 :: Int) Leaf))
      , MRSOP.geq (into (Base "test"))         (into (Qual "test" (Base "test"))
      , MRSOP.geq (into (BookInfo "a" "b" 1))  (into (BookInfo "a" "b" 2)
      , MRSOP.geq (into (BookInfo "a'" "b" 1)) (into (BookInfo "a" "b" 1)
      ]
    it "for BookInfo" $ property $ \x -> SOP.geq @BookInfo x x
    it "for QualName" $ property $ \x -> SOP.geq @QualName x x
    it "for Tree12"   $ property $ \x -> SOP.geq @(Tree12 Float) x x
-}
