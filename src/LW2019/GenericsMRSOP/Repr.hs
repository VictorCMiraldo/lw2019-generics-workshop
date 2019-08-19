{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs                 #-}
module LW2019.Generics.MRSOP.Repr where

import LW2019.Types.Regular
import Generics.MRSOP.Base
import Generics.MRSOP.Opaque
import Generics.MRSOP.TH

-- MRSOP also supports TH
deriveFamily  [t| BookInfo |]

-- let us write Family QualName by hand here

-- We don't support parameters though;
deriveFamily [t| Tree12 Int |]
