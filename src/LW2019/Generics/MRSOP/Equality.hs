{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE PolyKinds             #-}
module LW2019.Generics.MRSOP.Equality where

import LW2019.Types.Regular
import LW2019.Types.MutuallyRecursive
import LW2019.Generics.MRSOP.Repr

import Generics.MRSOP.Base hiding (geq)

-- The 'elimRep' combinator has type:
--
-- > elimRep :: (forall k. ki k -> a)
-- >         -> (forall (k :: Nat). IsNat k => f k -> a)
-- >         -> ([a] -> b)
-- >         -> Rep ki f c
-- >         -> b
--
-- And it is good to think of it in the following way:
--
-- > elimRep fK fI comb (C k1 k2 .. kn x1 x2 ... xn)
-- >   = comb [ fK k1 , ... , fK kn , fI x1 , ... fI xn ]

geq :: (EqHO ki , Family ki fam codes)
    => El fam ix -> El fam ix -> Bool
geq x y = go (dfrom x) (dfrom y)
  where
    go :: (EqHO ki)
       => Fix ki codes ix -> Fix ki codes ix -> Bool
    go (Fix x) (Fix y) = case zipRep x y of
      Nothing -> False
      Just r  -> elimRep
                   (uncurry' (==)) -- How to eliminate opaques
                   (uncurry' go)   -- How to eliminate recursive parts
                   and             -- How to combine the results
                   r               -- What to elminate over.

-- We can then instanteate 'geq' for different
-- families.
tree12geq :: Tree12 Int -> Tree12 Int -> Bool
tree12geq x y = geq (into @FamTree12Int x) (into @FamTree12Int y)

ldgeq :: LambdaLet Int -> LambdaLet Int -> Bool
ldgeq x y = geq (into @FamLambdaLetInt x) (into @FamLambdaLetInt  y)
