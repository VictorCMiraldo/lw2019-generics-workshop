{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module LW2019.Generics.GHC.Height where

import LW2019.Types.Regular
import LW2019.Types.Values
import LW2019.Generics.GHC.Repr

import GHC.Generics

import Data.Proxy

-- In the same spirit as Shape equality, we can do
-- generic heights.

class Height orig a where
  height :: Proxy orig -> a -> Int
  default height
    :: (Generic a , GHeight orig (Rep a))
    => Proxy orig -> a -> Int
  height p x = gheight p (from x) 

-- |Generic /Height/ class. Note how we must carry
-- around the type we are doing recursion over!
class GHeight orig (t :: * -> *) where
  gheight :: Proxy orig -> t x -> Int

instance GHeight orig U1 where
  gheight _ U1 = 0

instance (GHeight orig f , GHeight orig g)
    => GHeight orig (f :*: g) where
  gheight p (f :*: g)
    = max (gheight p f) (gheight p g)

instance (GHeight orig f , GHeight orig g)
    => GHeight orig (f :+: g) where
  gheight p (L1 f) = gheight p f
  gheight p (R1 g) = gheight p g

instance (GHeight orig f) => GHeight orig (M1 i s f) where
  gheight p (M1 x) = gheight p x

-- Constants have height zero
instance {-# OVERLAPPABLE #-} GHeight orig (K1 R a) where
  gheight _ _ = 0 

-- Unless it's the very datatype we are recursing over,
-- then we need to do the recursive step properly.
instance {-# OVERLAPPING #-} (Height orig orig)
    => GHeight orig (K1 R orig) where
  gheight p (K1 x) = 1 + height p x

instance Height BookInfo   BookInfo   where
instance Height QualName   QualName   where
instance Height (Tree12 a) (Tree12 a) where

heightQualName :: QualName -> Int
heightQualName = height (Proxy :: Proxy QualName)

heightTree12 :: forall a . Tree12 a -> Int
heightTree12 = height (Proxy :: Proxy (Tree12 a))

