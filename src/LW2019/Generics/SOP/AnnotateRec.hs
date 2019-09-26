{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module LW2019.Generics.SOP.AnnotateRec where

import LW2019.Types.Regular
import LW2019.Generics.SOP.Repr
import Generics.SOP

-- Shape equality uncovers downside of
-- not having explicit recursion. We will have to
-- resort to typeclasses to distinguish which bits of
-- the type are recursive from those that are not.
--
-- Unlike GHC.Generics, for SOP we can do this effort
-- once and for all.


-- The idea is that we will annotate a product type
-- with information about its fields beint recursive or not.
data Ann orig :: * -> * where
  Rec   :: orig -> Ann orig orig
  NoRec :: x    -> Ann orig x

class AnnotateRec orig (prod :: [ * ]) where
  annotate :: NP I prod -> NP (Ann orig) prod

instance AnnotateRec orig '[] where
  annotate Nil = Nil
  
instance {-# OVERLAPPABLE #-} (AnnotateRec orig xs)
    => AnnotateRec orig (x ': xs) where
  annotate (I x :* xs) = NoRec x :* (annotate xs)

instance {-# OVERLAPPING #-} (AnnotateRec orig xs)
    => AnnotateRec orig (orig ': xs) where
  annotate (I x :* xs) = Rec x :* (annotate xs)


