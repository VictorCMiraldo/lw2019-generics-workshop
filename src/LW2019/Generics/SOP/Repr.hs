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

instance Generic BookInfo where
  -- Only one constructor with three fields
  type Code BookInfo = '[ '[String , String , Float] ]

  to (SOP (Z (I name :* I isbn :* I price :* Nil)))
    = BookInfo name isbn price
  from (BookInfo name isbn price)
    = (SOP (Z (I name :* I isbn :* I price :* Nil)))


instance Generic QualName where
  type Code QualName = '[ '[String]
                        , '[String , QualName] ]

  to (SOP    (Z (I b :* Nil)))         = Base b
  to (SOP (S (Z (I b :* I r :* Nil)))) = Qual b r

  from (Base b)   = SOP    (Z (I b :* Nil))
  from (Qual b r) = SOP (S (Z (I b :* I r :* Nil)))

-- We can also use template haskell to derive our instances.
-- What's the generated code? Again, we can check by
-- running:
--
-- > :set -XRankNTypes
-- > :kind! forall a . Code (Tree12 a)
--
deriveGeneric ''Tree12
