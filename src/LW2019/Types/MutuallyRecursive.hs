-- |Here we will find regular datatypes to
-- experiment with. Some with recursion,
-- some without recursion.
module LW2019.Types.MutuallyRecursive where

-- |This is the simplest example of a "mutually recursive"
-- datatype (in fact, it's so simple we call it a 'nested' type... shhh)
-- This is handy to practice on, but pretty boring.
data Rose a = Rose a [Rose a]
  deriving (Show)

-- |A more realistic datatype would be that of
-- the lambda-calculus with let-statements.
data LambdaLet v
  = Var v
  | Abs v (LambdaLet v)
  | App (LambdaLet v) (LambdaLet v)
  | Let [LambdaDecl v] (LambdaLet v)
  deriving (Show)

data LambdaDecl v
  = Decl v (LambdaLet v)
  deriving (Show)
