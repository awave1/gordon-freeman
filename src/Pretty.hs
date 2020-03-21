module Pretty
  ( pretty
  , prettyDeBruijn
  )
where

import           LambdaSyntax

pretty :: Lambda -> String
pretty expr = case expr of
  (Var     name)     -> name
  (Literal lit )     -> showLit lit
  Nil                -> "Nil"
  (Cons el1   el2  ) -> "[" ++ pretty el1 ++ ", " ++ pretty el2 ++ "]"
  (App  expr1 expr2) -> printExpr pretty expr1 expr2 " "
  (Add  expr1 expr2) -> printExpr pretty expr1 expr2 " + "
  (Mul  expr1 expr2) -> printExpr pretty expr1 expr2 " * "
  (LEq  expr1 expr2) -> printExpr pretty expr1 expr2 " <= "
  (If cond ifExpr elseExpr) ->
    "(if ("
      ++ pretty cond
      ++ ") then "
      ++ pretty ifExpr
      ++ " else "
      ++ pretty elseExpr
      ++ ")"
  (Case cond c1 c2) ->
    "(case " ++ pretty cond ++ ": " ++ pretty c1 ++ " or " ++ pretty c2 ++ ")"

  (Abs name expr) -> "(" ++ "λ" ++ name ++ "." ++ pretty expr ++ ")"

prettyDeBruijn :: DeBruijnLambda -> String
prettyDeBruijn expr = case expr of
  (DVar     name) -> show name
  (DLiteral lit ) -> showLit lit
  DNil            -> "DNil"
  (DCons el1 el2) ->
    "[" ++ prettyDeBruijn el1 ++ ", " ++ prettyDeBruijn el2 ++ "]"
  (DApp expr1 expr2) -> printExpr prettyDeBruijn expr1 expr2 " "
  (DAdd expr1 expr2) -> printExpr prettyDeBruijn expr1 expr2 " + "
  (DMul expr1 expr2) -> printExpr prettyDeBruijn expr1 expr2 " * "
  (DLEq expr1 expr2) -> printExpr prettyDeBruijn expr1 expr2 " <= "
  (DIf cond ifExpr elseExpr) ->
    "(if ("
      ++ prettyDeBruijn cond
      ++ ") then "
      ++ prettyDeBruijn ifExpr
      ++ " else "
      ++ prettyDeBruijn elseExpr
      ++ ")"
  (DCase cond c1 c2) ->
    "(case "
      ++ prettyDeBruijn cond
      ++ ": "
      ++ prettyDeBruijn c1
      ++ " or "
      ++ prettyDeBruijn c2
      ++ ")"
  (DAbs expr) -> "(" ++ "λ" ++ "." ++ prettyDeBruijn expr ++ ")"

printExpr :: (a -> String) -> a -> a -> String -> String
printExpr print' expr1 expr2 sym = print' expr1 ++ sym ++ print' expr2

showLit :: Literal -> String
showLit lit = case lit of
  (LInt  i) -> show i
  (LBool b) -> show b
