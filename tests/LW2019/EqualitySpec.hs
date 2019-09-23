{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE KindSignatures   #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE PolyKinds        #-}
module LW2019.EqualitySpec (spec) where

import Data.Proxy

import LW2019.Types.Regular
import LW2019.Types.MutuallyRecursive
import LW2019.Types.Instances.Arbitrary

import qualified LW2019.Generics.GHC.Equality    as GHC
import qualified LW2019.Generics.SOP.Equality    as SOP
import qualified LW2019.Generics.MRSOP.Equality  as MRSOP

import GHC.Generics

import Test.QuickCheck
import Test.Hspec

spec :: Spec
spec = describe "Equality" $ do
  describe "Generics.GHC.Equality" $ do
    it "fails for select cases" $ property $ and $ map not
      [ GHC.eq Leaf                  (Node1 (3 :: Int) Leaf)
      , GHC.eq (Base "test")         (Qual "test" (Base "test"))
      , GHC.eq (BookInfo "a" "b" 1)  (BookInfo "a" "b" 2)
      , GHC.eq (BookInfo "a'" "b" 1) (BookInfo "a" "b" 1)
      ]
    it "for BookInfo" $ property $ \x -> GHC.eq @BookInfo x x
    it "for QualName" $ property $ \x -> GHC.eq @QualName x x
    it "for Tree12"   $ property $ \x -> GHC.eq @(Tree12 Int) x x


  describe "Generics.SOP.Equality" $ do
    it "retrofits GHC.Equality" $ property $
      \x y -> GHC.eq @(Tree12 Int) x y == SOP.geq x y

  describe "Generics.MRSOP.Equality" $ do
    it "retrofits GHC.Equality (Tree12 Int)" $ property $
      \x y -> MRSOP.tree12geq x y == GHC.eq x y
    it "for LambdaLet Int" $ property
      $ forAll (resize 14 arbitrary) $
      \x -> MRSOP.ldgeq x x

-- We can still use GHC.Generics-style equality for mutually
-- recursive types, we just have to manually derive all
-- the necessary instances.
deriving instance Generic (LambdaLet x)
deriving instance Generic (LambdaDecl x)
instance (GHC.Eq' x) => GHC.Eq' (LambdaLet x)
instance (GHC.Eq' x) => GHC.Eq' (LambdaDecl x)
instance (GHC.Eq' x) => GHC.Eq' [LambdaDecl x]

