-- |Here we will find regular datatypes to
-- experiment with. Some with recursion,
-- some without recursion.
module LW2019.Types.Regular where

-- |As our first example we look into a non-recursive
-- datatype, call it 'BookInfo'.
data BookInfo
  = BookInfo String -- ^ Name
             String -- ^ ISBN
             Float  -- ^ Price
  deriving (Show)

-- |Lets make a slightly more interesting datatype
-- for our second example: Fully qualified names
data QualName
  = Base String
  | Qual String QualName
  deriving (Show)

-- |Finally, a parametrized datatype with recursion!
data Tree12 a
  = Leaf
  | Node1 a (Tree12 a)
  | Node2 a (Tree12 a) (Tree12 a)
  deriving (Show)

