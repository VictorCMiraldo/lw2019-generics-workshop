module LW2019.GenericsMRSOP.Equality where

import LW2019.GenericsMRSOP.Repr

import Generics.MRSOP.Base

geq :: (EqHO ki , Family ki fam codes)
    => El fam ix -> El fam ix -> Bool
geq x y = go (dfrom x) (dfrom y)
  where
    go :: (EqHO ki)
       => Fix ki codes ix -> Fix ki codes ix -> Bool
    go (Fix x) (Fix y) = case zipRep x y of
      Nothing -> False
      Just r  -> elimRep (uncurry' eqHO)
                         (uncurry' go)
                         and
                         r
