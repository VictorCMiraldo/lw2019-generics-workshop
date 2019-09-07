{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}
module LW2019.Generics.MRSOP.Height where

import Data.Functor.Const

import LW2019.Types.Regular
import LW2019.Generics.MRSOP.Repr

import Generics.MRSOP.Base
import Generics.MRSOP.AG

-- Let's start computing heights recursively;
gheight0 :: (Family ki fam codes)
         => El fam ix -> Int
gheight0 x = go (dfrom x)
  where
    go :: Fix ki codes ix -> Int
    go x = case sop (unFix x) of
               Tag _ p -> maximum (0:elimNP recHeight p)

    recHeight :: NA ki (Fix ki codes) at -> Int
    recHeight (NA_K _) = 0
    recHeight (NA_I x) = 1 + go x

-- Well, like most recursive functions, computing the
-- height of a tree is a catamorphism.
--
-- For that we need an algebra.
heightAlg :: Rep ki (Const Int) sum -> Const Int iy
heightAlg = elimRep (const (Const 0))
                    (Const . (1+) . getConst)
                    auxMax
  where
    auxMax :: [Const Int x] -> Const Int x
    auxMax = Const . maximum . (0:) . map getConst

-- Using the Algebra is pretty simple
gheight1 :: (IsNat ix , Family ki fam codes)
         => El fam ix -> Int
gheight1 = getConst . cata heightAlg . dfrom

-- But wait... what if I keep needing the height of whatever
-- subtrees of some tree we are performing a computation? 
-- This is often the case with more involved algorithms.
-- We'd essentially like to annotate our trees
-- with the result of every

gheight2 :: (IsNat ix , Family ki fam codes)
         => El fam ix -> AnnFix ki codes (Const Int) ix
gheight2 = synthesize heightAlg . dfrom
