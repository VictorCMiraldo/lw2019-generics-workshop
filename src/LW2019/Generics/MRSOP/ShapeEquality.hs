{-# LANGUAGE PolyKinds        #-}
{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE TypeApplications #-}
module LW2019.Generics.MRSOP.ShapeEquality where

import LW2019.Types.Regular
import LW2019.Generics.MRSOP.Repr

import Generics.MRSOP.Base

gseq :: (EqHO ki , Family ki fam codes)
     => El fam ix -> El fam ix -> Bool
gseq x y = go (dfrom x) (dfrom y)
  where
    go :: (EqHO ki)
       => Fix ki codes ix -> Fix ki codes ix -> Bool
    go (Fix x) (Fix y) = case zipRep x y of
      Nothing -> False
      Just r  -> elimRep (const True)
                         (uncurry' go)
                         and
                         r

gbook1 :: El FamBookInfo Z
gbook1 = into book1

gbook2 = into @FamBookInfo book2

res = gseq gbook1 gbook2

