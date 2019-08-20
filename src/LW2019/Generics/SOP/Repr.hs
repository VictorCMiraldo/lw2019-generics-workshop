{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE TemplateHaskell    #-}
module LW2019.Generics.SOP.Repr where

-- The types we want to look at are:
import LW2019.Types.Regular

-- Our generic library of study here will be:
import Generics.SOP
import Generics.SOP.TH
import qualified GHC.Generics as GHC

-- * Generic Representation of Regular Types

-- Here we define all by hand
instance Generic BookInfo where
  -- Only one constructor with three fields
  type Code BookInfo = '[[String , String , Float]]

  to (SOP (Z (I name :* I isbn :* I price :* Nil)))
    = BookInfo name isbn price
  from (BookInfo name isbn price)
    = (SOP (Z (I name :* I isbn :* I price :* Nil)))

-- But hey: generics-sop has "Generic Generic Programming",
-- meaning that it can use GHC.Generics instances to
-- derive generics-sop 'Generic' instances:
deriving instance GHC.Generic QualName
instance Generic QualName 
-- This only works for automatically generated GHC.Generic
-- instances.


-- We can also use template haskell to derive our instances.
-- What's the generated code? Again, we can check by
-- running:
--
-- > :set -XRankNTypes
-- > :kind! forall a . Code (Tree12 a)
--
deriveGeneric ''Tree12
