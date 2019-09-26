{-# LANGUAGE KindSignatures   #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
module LW2019.Generics.SOP.Equality where

import LW2019.Types.Regular
import LW2019.Generics.SOP.Repr
import Generics.SOP

-- Recall the definition of NS and NP.

-- Consider that:
--
-- > type family All p '[]       = ()
-- > type family All p (c ': cs) = (p c , All p cs)
-- > 
-- > type family All2 p c        = All (All p) c
--

geq :: (Generic a , All2 Eq (Code a))
    => a -> a -> Bool
geq x y = go (from x) (from y)
  where
    go :: (All2 Eq xss) => SOP I xss -> SOP I xss -> Bool
    go (SOP (Z xs))  (SOP (Z ys))  = and . hcollapse $ hczipWith p eq xs ys
    go (SOP (S xss)) (SOP (S yss)) = go (SOP xss) (SOP yss)
    go _             _             = False

    p :: Proxy Eq
    p = Proxy

    eq :: forall (a :: *). Eq a => I a -> I a -> K Bool a
    eq (I a) (I b) = K (a == b)

-- Now, we need to instruct GHC to use the generic equality
-- whenever it is prompted with resolving an Eq constraint
-- for our types.
--
-- If you struggle to see why, dry-run 'geq (Node1 42 Leaf) (Node1 42 Leaf)'
-- on pen-and-paper, whilst keeping track of the instance resolution.

instance Eq QualName where
  x == y = geq x y

instance Eq a => Eq (Tree12 a) where
  x == y = geq x y
