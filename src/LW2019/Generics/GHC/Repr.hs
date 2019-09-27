{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE TypeOperators      #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE DefaultSignatures  #-}
{-# LANGUAGE KindSignatures     #-}
module LW2019.Generics.GHC.Repr where

-- The types we want to look at are:
import LW2019.Types.Regular

-- Our generic library of study here will be:
import GHC.Generics

-- * Generic Representation of Regular Types

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

-- Let's look into 'QualName':
instance Generic QualName where
  type Rep QualName = K1 R String :+: (K1 R String :*: K1 R QualName)

  to (L1 (K1 name))          = _ex2_a
  to (R1 (K1 q :*: K1 rest)) = _ex2_b

  from = _ex2_c

-- GHC can infer these boring instances for us, though:
deriving instance Generic (Tree12 a)

-- Check the representation of 'Tree12 a' by typing
-- the following command on your ghci:
--
-- > :kind! Rep (Tree12 Int)
--
-- Ugly, huh? Yeah, that's because GHC adds a lot of
-- metainformation there so we can access a number of interesting
-- things, ie, the name of the original constructors.
-- Can you distil a simpler way of writing that type?
-- Bear in mind that "Rec0" is just "K1 R":

type Tree12Rep a 
  = Ex2_D

data Ex2_D
