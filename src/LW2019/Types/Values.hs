module LW2019.Types.Values where

import LW2019.Types.Regular
import LW2019.Types.MutuallyRecursive
import LW2019.Types.Instances.Arbitrary

import System.IO.Unsafe

import Test.QuickCheck

genVal :: Arbitrary a => Int -> a
genVal n = unsafePerformIO $ generate (resize n arbitrary)

valBookInfo :: Int -> BookInfo
valBookInfo = genVal

valQualName :: Int -> QualName
valQualName = genVal

valTree12 :: Int -> Tree12 Int
valTree12 = genVal

valRose :: Int -> Rose Int
valRose = genVal

valLambdaLet :: Int -> LambdaLet Int
valLambdaLet = genVal

valLambdaDecl :: Int -> LambdaDecl Int
valLambdaDecl = genVal
