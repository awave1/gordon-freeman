module Pretty where

import           Data.Either
import           Data.List

import           Syntax

pretty :: Expr -> String
pretty expr = case expr of
  (Var name       ) -> name
  (App expr1 expr2) -> pretty expr1 ++ " " ++ pretty expr2
  (Lam name  expr ) -> "λ" ++ name ++ "." ++ pretty expr
  _                 -> ""


viewVars :: Expr -> [Name]
viewVars (Lam n a) = n : viewVars a
viewVars _         = []

viewBody :: Expr -> Expr
viewBody (Lam _ e) = viewBody e
viewBody x         = x

