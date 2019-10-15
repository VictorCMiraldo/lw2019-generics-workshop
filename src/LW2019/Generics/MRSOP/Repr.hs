{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs                 #-}
module LW2019.Generics.MRSOP.Repr where

import LW2019.Types.Regular
import LW2019.Types.MutuallyRecursive
import Generics.MRSOP.Base
import Generics.MRSOP.Opaque
import Generics.MRSOP.TH

-- MRSOP also supports TH
deriveFamily  [t| BookInfo |]

-- let us write Family QualName by hand here
type FamQualName_   = '[ QualName ]
type CodesQualName_ = '[ '[ '[K KString] , '[K KString , I Z]]]

-- Here and There are synonyms to SOP.Z and SOP.S; but since
-- we use natural numbers here, it can get confusing.

instance Family Singl FamQualName_ CodesQualName_ where
  sfrom' SZ (El (Base str))   = Rep $ Here (NA_K (SString str) :* Nil)
  sfrom' SZ (El (Qual str n)) = Rep $ There (Here ((NA_K (SString str))
                                               :* (NA_I (El n))
                                               :* Nil))

  sto' SZ (Rep (Here (NA_K (SString str) :* Nil))) = El (Base str)
  sto' SZ (Rep (There (Here (NA_K (SString str) :* NA_I (El n) :* Nil))))
    = El (Qual str n)

-- We don't support parameters though;
deriveFamily [t| Tree12 Int |]

-- Let's now define a more interesting one

-- |All codes packed in a type-level list
type CodesRoseInt = '[  '[ '[]                  , '[ 'I ('S 'Z) , 'I 'Z] ]
                     ,  '[ '[ K KInt , 'I 'Z ] ]
                     ]

-- |The types corresponding the the codes in 'CodesRose'
-- appear in the same order.
type FamRoseInt  = '[ [Rose Int] , Rose Int] 

instance Family Singl FamRoseInt CodesRoseInt where
  sfrom' (SS SZ) (El (Rose a as)) = Rep $ Here (NA_K _ex8_e :* NA_I (El as) :* Nil)
  sfrom' SZ (El [])              = Rep $ Here Nil
  sfrom' SZ (El (x:xs))          = Rep $ There (Here (NA_I _ex8_c :* NA_I _ex8_d :* Nil))

  sto' = _ex8_a

-- Or, say we actually want to have a particular selection
-- of opaque types, we can do so in a few easy steps:

-- 1) Enumerate the types we want available
data MyOpq = MyInt

-- 2) Create an interpretation back into Hask
data MyOpqI :: MyOpq -> * where
  Opq :: Int -> MyOpqI MyInt

instance ShowHO MyOpqI where
  showsPrecHO d (Opq i) = showParen (d > app_prec) $
    showString "Opq " . shows i
   where app_prec = 10
    
instance EqHO MyOpqI where
  eqHO (Opq i) (Opq j) = i == j

-- 3) Call 'deriveDamilyWith' passing the interpretation
-- of our selection of opaque types.
deriveFamilyWith ''MyOpqI [t| LambdaLet Int |]
  
