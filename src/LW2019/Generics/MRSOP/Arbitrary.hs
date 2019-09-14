{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- |This is an advanced module meant for if we have some extra time
-- or for your own enjoyment on a plane back home or a rainy day.
--
-- The goal of this module is to generate well behaved Arbitrary instances.
-- By well behaved we mean to have some constraint on the size of
-- the value we are about to generate.
module LW2019.Generics.MRSOP.Arbitrary where

import Data.Proxy

import Test.QuickCheck

import Generics.MRSOP.Base
import Generics.MRSOP.Util
import Generics.MRSOP.AG (AnnFix , synthesize)

import LW2019.Generics.MRSOP.Repr

-- First, we'll need lots of proxies and hence, we'll need some
-- proxy manipulation functions. Regard these as a way to help GHC's
-- typechecker. For example, ptail will let the typechecker know that
-- it must use tail of some specific list to resolve some instance
-- whereas without the proxies, it would error out of multiple candidates.

ptail :: Proxy (x ': xs) -> Proxy xs
ptail _ = Proxy

phead :: Proxy (x ': xs) -> Proxy x
phead _ = Proxy

pconstr :: Constr sum n -> Proxy (Lkup n sum)
pconstr _ = Proxy

-- * Generating Arbitrary Values
--
-- $arbitrarydocs
--
-- Specification: Define garbitrary, of type Gen (El fam ix), such
-- that it respects the /size/ of the generator and uses it to direct
-- how many levels of recursion we unfold.
--
-- Example:
--   > generate (resize 3 garbitrary) :: IO QualName -- At most length 3
--   > (Qual "2we" (Base "as"))
--
-- To to that, we must understand which constructors of a datatype
-- are the 'base' constructors, that is, do not have any recursive
-- occurence as a field. For the list case, it would be '[]'.
--
-- The 'EnumProd' typeclass will provide two functions, 'isRec' that
-- tells wether the list of fields of a constructor is recursive
-- and 'genProd' that will generate a product of said list given
-- a way to generate atoms.
--
-- The 'EnumConstr' on the other hand, will enumerate the constructors
-- of a datatype, but will also provide a way to enumerate just the
-- base constructors of a datatype.

-- | We start with 'EnumProd', by induction on @prod@, but with help from
-- @codes@ and @ki@. An important trick to pay attention to is
-- the 'EnumConstr' constraint on the argument to 'genProd'; this closes the
-- recursive loop by saying that we can enumerate the constructors of
-- whatever target type genProd will have to generate.
class EnumProd (ki :: kon -> *) (codes :: [[[Atom kon]]]) (prod :: [Atom kon])
   where
  isRec   :: Proxy ki -> Proxy codes -> Proxy prod -> Bool
  genProd :: Proxy ki -> Proxy codes -> Proxy prod
          -> (forall ix . (IsNat ix , EnumConstr ki codes (Lkup ix codes)) => Gen (f ix))
          -> Gen (NP (NA ki f) prod)

instance EnumProd ki codes '[] where
  isRec   _ _ _   = False
  genProd _ _ _ _ = return NP0

-- |When we find an @I xi@ on the list of fields, this is a recursive field.
instance (EnumConstr ki codes (Lkup xi codes) , IsNat xi , EnumProd ki codes xs)
    => EnumProd ki codes (I xi ': xs) where
  isRec   _  _  _   = True
  genProd pk pc p f = (:*) <$> (NA_I <$> f) <*> genProd pk pc (ptail p) f 

-- |Otherwise, it depends on the rest of the fields.
instance (Arbitrary (ki k) , EnumProd ki codes xs)
    => EnumProd ki codes (K k ': xs) where
  isRec   pk pc p   = isRec pk pc (ptail p)
  genProd pk pc p f = (:*) <$> (NA_K <$> arbitrary) <*> genProd pk pc (ptail p) f

-- |ExConstr is a heterogeneous list of constructors; that's necessary because 'Constr'
-- is a relation and a value of type @Constr sum 3@ specifies that 3 is a valid index
-- into @sum@. Hence, we'd like a list of type @forall n . Constr sum n@, which is
-- better handled by a GADT. Note how we carry important constraints inside!
data ExConstr (ki :: kon -> *) codes :: [[Atom kon]] -> * where
  ExConstr :: (IsNat n , EnumProd ki codes (Lkup n sum))
           => Constr sum n -> ExConstr ki codes sum

exConstrSuc :: ExConstr ki codes sum -> ExConstr ki codes (s ': sum)
exConstrSuc (ExConstr c) = ExConstr (CS c)

class EnumConstr ki codes (sum :: [[Atom kon]]) where
  enumConstr :: Proxy ki -> Proxy codes -> Proxy sum -> [ExConstr ki codes sum]
  enumBase   :: Proxy ki -> Proxy codes -> Proxy sum -> [ExConstr ki codes sum]

instance EnumConstr ki fam '[] where
  enumConstr _ _ _ = []
  enumBase   _ _ _ = []

instance (EnumProd ki codes x , EnumConstr ki codes xs)
   => EnumConstr ki codes (x ': xs) where
  enumConstr pk pc p = ExConstr CZ : map exConstrSuc (enumConstr pk pc $ ptail p)
  enumBase   pk pc p = let rest = map exConstrSuc (enumBase pk pc $ ptail p)
                        in if isRec pk pc (phead p)
                           then rest -- Don't add it to the list
                           else ExConstr CZ : rest

-- |After going through all this work, the 'garbitrary' is straight forward.
-- We must be careful, however, that some types have no base constructor.
-- Take the 'Rose' type, for example, it has only one constructor that
-- refers to another member of the family. Hence, we can't stop generating there.
garbitrary :: forall ki fam codes ix
            . ( Family ki fam codes , IsNat ix
              , EnumConstr ki codes (Lkup ix codes))
           => Gen (El fam ix)
garbitrary = sto <$> (sized go)
  where
    pc :: Proxy codes
    pc = Proxy
    
    pk :: Proxy ki
    pk = Proxy

    go :: (EnumConstr ki codes sum)
       => Int -> Gen (Rep ki (El fam) sum)
    go n = do
      let bases   = enumBase   pk pc (Proxy :: Proxy sum)
      let constrs = enumConstr pk pc (Proxy :: Proxy sum)
      let avail   = if n == 0 && length bases > 0 -- What if @sum@ has no base case?
                    then bases
                    else constrs              -- We use the any other option
      let n'      = if n <= 0 then 0 else n-1 -- Making sure to never ask for a negative sized value
      (ExConstr c) <- elements avail
      inj c <$> genProd pk pc (pconstr c) (resize n' garbitrary)

-- * Interesting Exercises
--
-- $arbitraryexecises
--
-- We can adapt this code in a number of ways to eventually provide
-- a library that uses mrsop and generates arbitrary values. Actually,
-- if you are interested in that, contact me! :)
--
-- Anyway, here are a couple interesting exercises:
--
-- 1. Make a @garbitraryFixed@ function that generates elements
-- of /exactly/ the size given.
--
-- 2. Make a @garbitraryDistr@ function that receives a target distribution
-- of frequencies of the constructors to tweak how they are sampled from the list.
