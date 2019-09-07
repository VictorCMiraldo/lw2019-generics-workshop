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

-- Shape equality will suffer from a downside of
-- a combinator based approach. We will have to
-- resort to typeclasses in anyway, otherwise
-- we can't distinguish which fields should be recursed over.
--
-- That being said, the typeclass monsters can be used only
-- to identify the recursive positions, once that's done,
-- we can go back to using a combinator-based approach.
--
-- Moreover, this is more reusable than GHC.Generics.
-- We are using the same mechanism for ShapeEquality and Height.
-- With GHC.Generics we had to duplicate the effort of
-- deciding whether a type was an occurence of a recursive
-- type or not.

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


