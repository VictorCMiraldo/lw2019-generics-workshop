{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs                 #-}
module LW2019.GenericsMRSOP.Repr where

import LW2019.Types.Regular
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
