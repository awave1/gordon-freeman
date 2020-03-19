module Syntax where

type Name = String

data Lambda
  = Var String
  | App Lambda Lambda
  | Abs String Lambda -- basic Lambda Term
  | Add Lambda Lambda
  | Sub Lambda Lambda
  | Mul Lambda Lambda
  | Literal Literal
  | Cond Lambda Lambda Lambda -- main control flow, if
  deriving (Show, Eq)

data DeBruijnLambda
  = DVar Int
  | DApp DeBruijnLambda DeBruijnLambda
  | DAbs DeBruijnLambda
  | DAdd DeBruijnLambda DeBruijnLambda
  | DSub DeBruijnLambda DeBruijnLambda
  | DMul DeBruijnLambda DeBruijnLambda
  | DLiteral Literal
  | DCond DeBruijnLambda DeBruijnLambda DeBruijnLambda
  deriving (Show, Eq)

data Literal
  = LInt Int
  | LBool Bool
  deriving (Show, Eq, Ord)
