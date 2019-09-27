{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE DataKinds          #-}
-- I'll cheat and use the generic instances we defined
-- to get Arbitrary instances and run the test-suite! ;P
module LW2019.Types.Instances.Arbitrary where

import LW2019.Types.Regular
import LW2019.Types.MutuallyRecursive
import LW2019.Generics.MRSOP.Repr 
import LW2019.Generics.MRSOP.Arbitrary

import Generics.MRSOP.Base
import Generics.MRSOP.Opaque

import Test.QuickCheck

instance Arbitrary (Singl 'KString) where
  arbitrary = SString <$> arbitrary

instance Arbitrary (Singl 'KFloat) where
  arbitrary = SFloat <$> arbitrary

instance Arbitrary (Singl 'KInt) where
  arbitrary = SInt <$> arbitrary

instance Arbitrary (MyOpqI 'MyInt) where
  arbitrary = Opq <$> arbitrary

instance Arbitrary BookInfo where
  arbitrary = unEl <$> garbitrary @Singl @FamBookInfo @_ @Z

instance Arbitrary QualName where
  arbitrary = unEl <$> garbitrary @Singl @FamQualName_ @_ @Z

instance Arbitrary (Tree12 Int) where
  arbitrary = unEl <$> garbitrary @Singl @FamTree12Int @_ @Z

instance Arbitrary (LambdaLet Int) where
  arbitrary = unEl <$> garbitrary @MyOpqI @FamLambdaLetInt @_ @Z

instance Arbitrary (LambdaDecl Int) where
  arbitrary = unEl <$> garbitrary @MyOpqI @FamLambdaLetInt @_ @(S (S Z))

instance Arbitrary (Rose Int) where
  arbitrary = unEl <$> garbitrary @Singl @FamRoseInt @_ @(S Z)
