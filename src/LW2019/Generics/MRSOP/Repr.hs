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

instance Family Singl FamQualName_ CodesQualName_ where
  sfrom' SZ (El (Base str))   = Rep $ Here (NA_K (SString str) :* NP0)
  sfrom' SZ (El (Qual str n)) = Rep $ There (Here ((NA_K (SString str))
                                               :* (NA_I (El n))
                                               :* NP0))

  sto' SZ (Rep (Here (NA_K (SString str) :* NP0))) = El (Base str)
  sto' SZ (Rep (There (Here (NA_K (SString str) :* NA_I (El n) :* NP0))))
    = El (Qual str n)

-- We don't support parameters though;
deriveFamily [t| Tree12 Int |]

-- But we support fancier types just like that!
deriveFamily [t| Rose Int |]

-- Or, say we actually want to have a particular selection
-- of opaque types, we can do so in a few easy steps:

-- 1) Enumerate the types we want available
data MyOpq = MyInt

-- 2) Create an interpretation back into Hask
data MyOpqI :: MyOpq -> * where
  Opq :: Int -> MyOpqI MyInt

-- 3) Call 'deriveDamilyWith' passing the interpretation
-- of our selection of opaque types.
deriveFamilyWith ''MyOpqI [t| LambdaLet Int |]
  
