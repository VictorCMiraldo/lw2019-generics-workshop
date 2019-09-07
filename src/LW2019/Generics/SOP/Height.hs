{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module LW2019.Generics.SOP.Height where

import LW2019.Types.Regular
import LW2019.Generics.SOP.Repr
import LW2019.Generics.SOP.AnnotateRec

import Generics.SOP
import Generics.SOP.NS
import Generics.SOP.NP

gheight :: forall a . (Generic a , All (AnnotateRec a) (Code a))
        => a -> Int
gheight x = maximum . (0:)
          $ collapse_NS
          $ cmap_NS (Proxy :: Proxy (AnnotateRec a))
                    (K . go . annotate)
                    (unSOP $ from x)
  where
    go :: NP (Ann a) xs -> [Int]
    go Nil             = []
    go (Rec   x :* xs) = 1 + gheight x : go xs
    go (NoRec x :* xs) = 0             : go xs
