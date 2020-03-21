module LambdaSyntax where

type Name = String

data Lambda
  = Var String
  | Literal Literal
  | App Lambda Lambda
  | Abs String Lambda -- basic Lambda Term
  | Add Lambda Lambda
  | Mul Lambda Lambda
  | LEq Lambda Lambda
  | Nil
  | Cons Lambda Lambda
  | If Lambda Lambda Lambda -- main control flow, if
  | Case Lambda Lambda Lambda
  | Fix Lambda
  deriving (Show, Eq)

data DeBruijnLambda
  = DVar Int
  | DLiteral Literal
  | DApp DeBruijnLambda DeBruijnLambda
  | DAbs DeBruijnLambda
  | DAdd DeBruijnLambda DeBruijnLambda
  | DSub DeBruijnLambda DeBruijnLambda
  | DMul DeBruijnLambda DeBruijnLambda
  | DLEq DeBruijnLambda DeBruijnLambda
  | DNil
  | DCons DeBruijnLambda DeBruijnLambda
  | DIf DeBruijnLambda DeBruijnLambda DeBruijnLambda -- main control flow, if
  | DCase DeBruijnLambda DeBruijnLambda DeBruijnLambda
  | DFix DeBruijnLambda
  deriving (Show, Eq)

data Literal
  = LInt Int
  | LBool Bool
  deriving (Show, Eq, Ord)
