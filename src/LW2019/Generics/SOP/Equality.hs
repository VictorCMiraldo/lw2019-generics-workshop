{-# LANGUAGE KindSignatures   #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
module LW2019.Generics.SOP.Equality where

import LW2019.Types.Regular
import LW2019.Generics.SOP.Repr
import Generics.SOP

-- Recall the definition of NS and NP.

geq :: (Generic a , All2 Eq (Code a))
    => a -> a -> Bool
geq x y = go (from x) (from y)
  where
    go :: forall xss. (All2 Eq xss, All SListI xss)
       => SOP I xss -> SOP I xss -> Bool
    go (SOP (Z xs))  (SOP (Z ys))  = and . hcollapse $ hcliftA2 p eq xs ys
    go (SOP (S xss)) (SOP (S yss)) = go (SOP xss) (SOP yss)
    go _             _             = False

    p :: Proxy Eq
    p = Proxy

    eq :: forall (a :: *). Eq a => I a -> I a -> K Bool a
    eq (I a) (I b) = K (a == b)

instance Eq QualName where
  x == y = geq x y
