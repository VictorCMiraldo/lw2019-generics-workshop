{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
module LW2019.Prelude where

-- |Take a simple Binary Tree type:
data Bin a = Leaf | Fork a (Bin a) (Bin a)
  deriving (Eq , Show)

-- |The art of generic programming consists
-- in representing this type by the composition of
-- simple combinators, that can be handled in
-- a generic fashion.
--
-- Actually, we can represent any datatype using only
-- Either; (,); (); and an assormtnet of opaque types
-- such as Int, Char, etc...
--
type Bin' a = Either () (a , Bin a , Bin a)

to :: Bin' a -> Bin a
to (Left ())         = Leaf
to (Right (a, l, r)) = Fork a l r

from :: Bin a -> Bin' a
from Leaf         = Left ()
from (Fork a l r) = Right (a , l , r)

-- * Recursion

-- |Recursive datatypes are nothing but the least
-- fixpoint of some functor. In our case, 'Bin'' above
-- is the functor; we make it into a newtype so that we can partially apply it.
newtype BinF a x = BinF { unBinF :: Either () (a , x , x) }
  deriving (Show , Functor)

newtype Fix f    = Fix { unFix :: f (Fix f) }
deriving instance (Show (f (Fix f))) => Show (Fix f)

-- |A deep conversion will convert the entire tree to its
-- generic representation.
deep_from :: Bin a -> Fix (BinF a)
deep_from = Fix . fmap deep_from . BinF . from

deep_to :: Fix (BinF a) -> Bin a
deep_to = to . unBinF . fmap deep_to . unFix


-- Play around; load the module and call the conversion functions.
--
-- Is it the case that to (from x) == x?
--
t1 , t2 :: Bin Int
t1 = Bin 42 Leaf Leaf
t2 = Bin 21 t1 Leaf
