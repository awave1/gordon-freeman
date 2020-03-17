module Pretty where

import           Syntax

pretty :: Expr -> String
pretty expr = case expr of
  (Var name        ) -> name
  (Lit (LInt  i   )) -> show i
  (Lit (LBool bool)) -> show bool
  (App expr1 expr2 ) -> pretty expr1 ++ " " ++ pretty expr2
  (Lam name  expr  ) -> "λ" ++ name ++ "." ++ pretty expr
