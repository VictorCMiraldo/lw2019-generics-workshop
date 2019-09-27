{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}
module LW2019.Generics.MRSOP.Height where

import Data.Proxy
import Data.Functor.Const

import LW2019.Types.Regular
import LW2019.Types.Values
import LW2019.Generics.MRSOP.Repr

import Generics.MRSOP.Base
import Generics.MRSOP.Opaque
import Generics.MRSOP.AG

import Data.SOP.Constraint

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
-- height of a tree is a catamorphism (aka fold).
--
-- For that we need an algebra that given a generic representation
-- of a type where the height of the recursive parts have already been
-- computed, assemblesthis into a new height.
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

-- To Play Around:
--

-- Here's an instantiated version of gheight2
gheight2Tree12 :: Tree12 Int
               -> AnnFix Singl CodesTree12Int (Const Int) Z
gheight2Tree12 = gheight2 . into


-- And an ad-hoc show instance to see what's going on. in a slightly
-- clearer fashion
prettyfy :: forall x . (IsNat x)
         => AnnFix Singl CodesTree12Int (Const Int) x
         -> [String]
prettyfy (AnnFix (Const h) d) = 
  case go prettyfy (datatypeInfo proxyFam snatX) d of
    []     -> []
    (x:xs) -> ("[h = " ++ show h ++ "] " ++ x) : map ("  " ++) xs
  where
    proxyFam :: Proxy FamTree12Int
    proxyFam = Proxy

    snatX = getSNat (Proxy :: Proxy x) 
    
    go :: (forall x . (IsNat x) => f x -> [String])
       -> DatatypeInfo sum
       -> Rep Singl f sum
       -> [String]
    go pf di x = case sop x of
      Tag c p -> let ci = constrInfoLkup c di
                  in constructorName ci : map (' ':) (go' pf p)
      
    go' :: (forall x . (IsNat x) => f x -> [String])
        -> PoA Singl f prod -> [String]
    go' pf Nil = []
    go' pf (NA_K k :* xs) = show k : go' pf xs
    go' pf (NA_I x :* xs) = pf x ++ go' pf xs
         
instance (IsNat x) => Show (AnnFix Singl CodesTree12Int (Const Int) x) where
  show = unlines . prettyfy
