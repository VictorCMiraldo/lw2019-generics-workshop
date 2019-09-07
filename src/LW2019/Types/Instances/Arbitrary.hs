{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
-- I'll cheat and use the generic instances we defined
-- to get Arbitrary instances and run the test-suite! ;P
module LW2019.Types.Instances.Arbitrary where

import LW2019.Types.Regular
import LW2019.Types.MutuallyRecursive
import LW2019.Generics.GHC.Repr () -- Bring only instances

import GHC.Generics

import Generic.Random
import Test.QuickCheck

-- Great example of shape problems; manual instances
-- can't have arbitrary derived automatically

instance Arbitrary BookInfo where
  arbitrary = BookInfo <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary QualName where
  arbitrary = sized $ \n ->
    case n of
      0 -> Base <$> arbitrary
      m -> oneof [ Base <$> arbitrary
                 , Qual <$> arbitrary
                        <*> resize (n-1) arbitrary ]

instance Arbitrary a => Arbitrary (Tree12 a) where
  arbitrary = genericArbitraryU

deriving instance Generic (Rose a)
deriving instance Generic (LambdaLet a)
deriving instance Generic (LambdaDecl a)

instance Arbitrary a => Arbitrary (Rose a) where
  arbitrary = genericArbitraryU

instance Arbitrary a => Arbitrary (LambdaLet a) where
  arbitrary = genericArbitraryU

instance Arbitrary a => Arbitrary (LambdaDecl a) where
  arbitrary = genericArbitraryU


