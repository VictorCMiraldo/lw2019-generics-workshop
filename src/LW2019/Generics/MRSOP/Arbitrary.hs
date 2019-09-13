{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE QuantifiedConstraints #-}
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
module LW2019.Generics.MRSOP.Arbitrary where

import Data.Proxy

import Test.QuickCheck

import Generics.MRSOP.Base
import Generics.MRSOP.Util
import Generics.MRSOP.AG (AnnFix , synthesize)

import LW2019.Generics.MRSOP.Repr

import Debug.Trace

class ArbitraryHO f x where
  garbitraryHO :: Gen (f x)

ptail :: Proxy (x ': xs) -> Proxy xs
ptail _ = Proxy

phead :: Proxy (x ': xs) -> Proxy x
phead _ = Proxy

pconstr :: Constr sum n -> Proxy (Lkup n sum)
pconstr _ = Proxy

class IsRec (ki :: kon -> *) (codes :: [[[Atom kon]]]) (prod :: [Atom kon])
   where
  isRec   :: Proxy ki -> Proxy codes -> Proxy prod -> Bool
  genProd :: Proxy ki -> Proxy codes -> Proxy prod
          -> (forall ix . (IsNat ix , EnumConstr ki codes (Lkup ix codes)) => Gen (f ix))
          -> Gen (NP (NA ki f) prod)

instance (EnumConstr ki codes (Lkup xi codes) , IsNat xi , IsRec ki codes xs)
    => IsRec ki codes (I xi ': xs) where
  isRec   _ _ _ = True
  genProd pk pc p f = (:*) <$> (NA_I <$> f) <*> genProd pk pc (ptail p) f 

instance IsRec ki codes '[] where
  isRec   _ _ _ = False
  genProd _ _ _ _ = return NP0

instance (Arbitrary (ki k) , IsRec ki codes xs)
    => IsRec ki codes (K k ': xs) where
  isRec   pk pc p = isRec pk pc (ptail p)
  genProd pk pc p f = (:*) <$> (NA_K <$> arbitrary) <*> genProd pk pc (ptail p) f

data ExConstr (ki :: kon -> *) codes :: [[Atom kon]] -> * where
  ExConstr :: (IsNat n , IsRec ki codes (Lkup n sum))
           => Constr sum n -> ExConstr ki codes sum

class EnumConstr ki codes (sum :: [[Atom kon]]) where
  enumConstr :: Proxy ki -> Proxy codes -> Proxy sum -> [ExConstr ki codes sum]
  enumBase   :: Proxy ki -> Proxy codes -> Proxy sum -> [ExConstr ki codes sum]

instance EnumConstr ki fam '[] where
  enumConstr _ _ _ = []
  enumBase   _ _ _ = []

instance (IsRec ki codes x , EnumConstr ki codes xs)
   => EnumConstr ki codes (x ': xs) where
  enumConstr pk pc p = ExConstr CZ
                     : map (\(ExConstr c) -> ExConstr (CS c))
                           (enumConstr pk pc $ ptail p)
  enumBase pk pc p
    | not (isRec pk pc (phead p))
      = ExConstr CZ : map (\(ExConstr c) -> ExConstr (CS c))
                          (enumBase pk pc $ ptail p)
    | otherwise = map (\(ExConstr c) -> ExConstr (CS c))
                      (enumBase pk pc $ ptail p)

garbitrary :: forall ki fam codes ix
            . (Family ki fam codes , IsNat ix
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
    go n 
      | n <= 0 = do
        let cs = enumBase   pk pc (Proxy :: Proxy sum)
        -- gotta watch out for cs being the empty list. Some
        -- mutually recursive types have no "base" constructor; Take
        -- The famous rose-tree example.
        let cs' = if length cs == 0
                  then enumConstr pk pc (Proxy :: Proxy sum)
                  else cs
        (ExConstr c) <- oneof (map return $ cs')
        inj c <$> genProd pk pc (pconstr c) garbitrary
      | otherwise = do
        let cs = enumConstr pk pc (Proxy :: Proxy sum)
        (ExConstr c) <- oneof (map return $ cs)
        inj c <$> genProd pk pc (pconstr c) (resize (n-1) garbitrary) 
              
    
