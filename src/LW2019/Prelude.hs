module LW2019.Prelude where

-- |Take a simple Binary Tree type:
data Bin a = Leaf | Fork a (Bin a) (Bin a)
  deriving (Eq , Show)

-- * One Layer at a Time

-- |The art of generic programming consists
-- in representing this type by the composition of
-- simple combinators, that can be handled in
-- a generic fashion:
type Bin' a = Either () (a , Bin a , Bin a)

to :: Bin' a -> Bin a
to (Left ())         = Leaf
to (Right (a, l, r)) = Fork a l r

from :: Bin a -> Bin' a
from = undefined

-- No recursion have happened, so far, though.

-- * Recursion

-- |Recursive datatypes are nothing but the least
-- fixpoint of some functor. In our case, 'Bin' above
-- can be seen as @Fix (BinF a)@,
newtype BinF a x = BinF { unBinF :: Either () (a , x , x) }
newtype Fix f    = Fix { unFix :: f (Fix f) }

binToFixBinF :: Bin a -> Fix (BinF a)
binToFixBinF Leaf         = Fix (BinF (Left ()))
binToFixBinF (Fork a l r) = undefined

fixBinFToBin :: Fix (BinF a) -> Bin a
fixBinFToBin (Fix (BinF (Left ())))           = Leaf
fixBinFToBin (Fix (BinF (Right (a , l , r)))) = undefined
