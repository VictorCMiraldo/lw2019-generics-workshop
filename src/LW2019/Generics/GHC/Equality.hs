{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE TypeOperators      #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE DefaultSignatures  #-}
{-# LANGUAGE KindSignatures     #-}
module LW2019.Generics.GHC.Equality where

-- The types we want to look at are:
import LW2019.Types.Regular
import LW2019.Types.Values
import LW2019.Generics.GHC.Repr

-- Our generic library of study here will be:
import GHC.Generics

-- Here we shall define equality for a range of types.
-- The @default@ keyword instructs GHC to use that
-- implementation when none is provided. It gets
-- triggered when declaring instances like:
--
-- > instance Eq' YourType
--
class Eq' a where
  eq :: a -> a -> Bool
  -- Note that when using the default implementation, we require
  -- the type to be an instance of Generic. The GEq' class will be
  -- defined for ALL possible representations.
  default eq
     :: (Generic a , GEq' (Rep a))
     => a -> a -> Bool
  eq x y = geq (from x) (from y)

-- The generic equality is then defined by induction
-- on the /structure/ of the representation.
class GEq' (f :: * -> *) where
  geq :: f x -> f y -> Bool

-- Base Case: Unit type
instance GEq' U1 where
  geq U1 U1 = _ex3_a

-- Inductive case: Assuming f and g have equality;
-- how do we compare values of type (f :*: g) for equality?
instance (GEq' f , GEq' g) => GEq' (f :*: g) where
  geq (f1 :*: g1) (f2 :*: g2) = _ex3_b

-- Inductive case: Assuming f and g have equality;
-- how do we compare values of type (f :+: g) for equality?
instance (GEq' f , GEq' g) => GEq' (f :+: g) where
  geq = _ex3_c

-- Meta Information is simply ignored
instance (GEq' f) => GEq' (M1 i s f) where
  geq (M1 x) (M1 y) = geq x y

-- Tying the knot: Note how we ask an Eq' constraint for the type a
instance (Eq' a) => GEq' (K1 R a) where
  geq (K1 x) (K1 y) = _ex3_d


-- Now we declare some instances for our base types
instance Eq' String where
  eq = (==)

instance Eq' Float where
  eq = (==)

instance Eq' Int where
  eq = (==)

-- And we are ready to use the generic machinery
-- for our regular types!
instance Eq' BookInfo where
instance Eq' QualName where

-- Note how type parameters are a non-issue as long
-- as we require a comprassion instance for them.
instance Eq' a => Eq' (Tree12 a) where

-- In order to play around:

test1 :: Bool
test1 =
  let x = valTree12 4 -- gets a tree at most 4 constructors deep
   in eq x x
