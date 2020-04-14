module LambdaSyntax where


type Name = String

{-|
  Data type for extended lambda calculus
|-}
data Lambda
  = Var String
  | Literal Literal
  | App Lambda Lambda
  | Abs String Lambda -- basic Lambda Term
  | Add Lambda Lambda
  | Mul Lambda Lambda
  | LEq Lambda Lambda
  | GEq Lambda Lambda
  | Nil
  | Cons Lambda Lambda
  | If Lambda Lambda Lambda -- main control flow, if
  | Case Lambda Lambda Lambda
  | Fix Lambda
  deriving (Eq)

{-|
  Data type for extended lambda calculus, to be able to represent it in de Bruijn notation
|-}
data DeBruijnLambda
  = DVar Int
  | DLiteral Literal
  | DApp DeBruijnLambda DeBruijnLambda
  | DAbs DeBruijnLambda
  | DAdd DeBruijnLambda DeBruijnLambda
  | DSub DeBruijnLambda DeBruijnLambda
  | DMul DeBruijnLambda DeBruijnLambda
  | DLEq DeBruijnLambda DeBruijnLambda
  | DGEq DeBruijnLambda DeBruijnLambda
  | DNil
  | DCons DeBruijnLambda DeBruijnLambda
  | DIf DeBruijnLambda DeBruijnLambda DeBruijnLambda -- main control flow, if
  | DCase DeBruijnLambda DeBruijnLambda DeBruijnLambda
  | DFix DeBruijnLambda
  deriving (Eq)

data Literal
  = LInt Int
  | LBool Bool
  deriving (Eq, Ord)
