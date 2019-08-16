{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE KindSignatures    #-}
module LW2019.GenericsGHC.Equality where

-- The types we want to look at are:
import LW2019.Types.Regular

-- Our generic library of study here will be:
import GHC.Generics

-- |First and foremost; lets define our 'BookInfo' datatype
-- as an instance of 'Generic'.
--
-- We need three pieces:
--  1. The representation of BookInfo as a pattern functor
--  2. The 'to' function
--  3. The 'from' function
instance Generic BookInfo where
  type Rep BookInfo = K1 R String :*: K1 R String :*: K1 R Float

  to (K1 name :*: K1 isbn :*: K1 price)
    = BookInfo name isbn price
  from (BookInfo name isbn price)
    = K1 name :*: K1 isbn :*: K1 price

-- This example was easy though, no recursion whatosever.

-- |Here we shall define equality for a range of types.
-- The @default@ keyword shows the "for free" implementation,
-- to access it, use:
--
-- > instance Eq' YourType
--
class Eq' a where
  eq :: a -> a -> Bool
  default eq
     :: (Generic a , GEq' (Rep a))
     => a -> a -> Bool
  eq x y = geq (from x) (from y)

-- Now we declare some instances for our base types
instance Eq' String where
  eq = (==)

instance Eq' Float where
  eq = (==)

-- And we are ready to use the generic machinery
-- for BookInfo
instance Eq' BookInfo where
  


class GEq' (f :: * -> *) where
  geq :: f x -> f y -> Bool

instance GEq' U1 where
  geq U1 U1 = True

instance (GEq' f , GEq' g) => GEq' (f :*: g) where
  geq (f1 :*: g1) (f2 :*: g2) = geq f1 f2 && geq g1 g2

instance (GEq' f , GEq' g) => GEq' (f :+: g) where
  geq (L1 f1) (L1 f2) = geq f1 f2
  geq (R1 g1) (R1 g2) = geq g1 g2
  geq _       _       = False

instance (Eq' a) => GEq' (K1 R a) where
  geq (K1 x) (K1 y) = eq x y
