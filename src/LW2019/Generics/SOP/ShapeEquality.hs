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
module LW2019.Generics.SOP.ShapeEquality where

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


gshapeEq :: forall a . (Generic a , All (AnnotateRec a) (Code a))
         => a -> a -> Bool
gshapeEq x y = go (from x) (from y)
  where
    go :: forall xss. (All (AnnotateRec a) xss, All SListI xss)
       => SOP I xss -> SOP I xss -> Bool
    go (SOP (Z xs))  (SOP (Z ys))  = and . hcollapse
                                   $ hliftA2 seq (annotate xs)
                                                 (annotate ys)
    go (SOP (S xss)) (SOP (S yss)) = go (SOP xss) (SOP yss)
    go _             _             = False

    seq :: Ann a b -> Ann a b -> K Bool b
    seq (Rec x) (Rec y) = K (gshapeEq x y)
    seq _       _       = K True


