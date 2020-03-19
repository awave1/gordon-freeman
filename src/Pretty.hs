module Pretty
  ( pretty
  , prettyDeBruijn
  )
where

import           Syntax

pretty :: Lambda -> String
pretty expr = case expr of
  (Var     name   ) -> name
  (Literal lit    ) -> show lit
  (App expr1 expr2) -> pretty expr1 ++ " " ++ pretty expr2
  (Add expr1 expr2) -> pretty expr1 ++ " + " ++ pretty expr2
  (Abs name  expr ) -> "λ" ++ name ++ "." ++ pretty expr

prettyDeBruijn :: DeBruijnLambda -> String
prettyDeBruijn expr = case expr of
  (DVar     name   ) -> show name
  (DLiteral lit    ) -> show lit
  (DApp expr1 expr2) -> prettyDeBruijn expr1 ++ " " ++ prettyDeBruijn expr2
  (DAdd expr1 expr2) -> prettyDeBruijn expr1 ++ " + " ++ prettyDeBruijn expr2
  (DAbs expr       ) -> "λ" ++ "." ++ prettyDeBruijn expr
