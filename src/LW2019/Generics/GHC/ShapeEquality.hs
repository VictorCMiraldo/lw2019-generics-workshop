{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module LW2019.Generics.GHC.ShapeEquality where

import LW2019.Types.Regular
import LW2019.Types.Values
import LW2019.Generics.GHC.Repr

import GHC.Generics

import Data.Proxy

-- Just like equality; but now we keep track of the
-- datatype we are defining shape equality over, so that
-- we can recognize recursive bits.
class ShapeEq orig a where
  -- We need to pass a proxy so that the typechecker can
  -- distinuish between different orig.
  shapeEq :: Proxy orig -> a -> a -> Bool
  default shapeEq
    :: (Generic a , GShapeEq orig (Rep a))
    => Proxy orig -> a -> a -> Bool
  shapeEq p x y = gshapeEq p (from x) (from y)

-- The generic class is the same
class GShapeEq orig (t :: * -> *) where
  gshapeEq :: Proxy orig -> t x -> t y -> Bool

instance GShapeEq orig U1 where
  gshapeEq _ U1 U1 = True

instance (GShapeEq orig f , GShapeEq orig g)
    => GShapeEq orig (f :*: g) where
  gshapeEq p (f1 :*: g1) (f2 :*: g2)
    = gshapeEq p f1 f2 && gshapeEq p g1 g2

instance (GShapeEq orig f , GShapeEq orig g)
    => GShapeEq orig (f :+: g) where
  gshapeEq p (L1 f1) (L1 f2) = gshapeEq p f1 f2
  gshapeEq p (R1 g1) (R1 g2) = gshapeEq p g1 g2
  gshapeEq _ _       _       = False

instance (GShapeEq orig f) => GShapeEq orig (M1 i s f) where
  gshapeEq p (M1 x) (M1 y) = gshapeEq p x y

-- Constants always have the same shape!
instance {-# OVERLAPPABLE #-} GShapeEq orig (K1 R a) where
  gshapeEq _ _ _ = True

-- Unless it's the very datatype we are recursing over,
-- then we need to do the recursion.
instance {-# OVERLAPPING #-} (ShapeEq orig orig)
    => GShapeEq orig (K1 R orig) where
  gshapeEq p (K1 x) (K1 y) = shapeEq p x y

instance ShapeEq BookInfo BookInfo     where
instance ShapeEq QualName QualName     where
instance ShapeEq (Tree12 a) (Tree12 a) where

