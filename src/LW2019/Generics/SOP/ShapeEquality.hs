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
import LW2019.Generics.SOP.AnnotateRec
import Generics.SOP

-- Keep in mind:
--
-- > hliftA2 :: (forall a. f a -> g a -> h a)
-- >         -> NP f xs -> NP g xs -> NP h xs


shapeEq :: forall a . (Generic a , All (AnnotateRec a) (Code a))
        => a -> a -> Bool
shapeEq x y = go (from x) (from y)
  where
    go :: forall xss. (All (AnnotateRec a) xss, All SListI xss)
       => SOP I xss -> SOP I xss -> Bool
    go (SOP (Z xs))  (SOP (Z ys))  = and . hcollapse
                                   $ hliftA2 seq (annotate xs)
                                                 (annotate ys)
    go (SOP (S xss)) (SOP (S yss)) = go (SOP xss) (SOP yss)
    go _             _             = False

    seq :: Ann a b -> Ann a b -> K Bool b
    seq (Rec x) (Rec y) = K (shapeEq x y)
    seq _       _       = K True


