module DeBruijnConversion
  ( debruijnConvert
  )
where

import           Data.List
import           Data.Maybe

import           LambdaSyntax

{-|
  Perform De Bruijn conversion
|-}
debruijnConvert :: Lambda -> DeBruijnLambda
debruijnConvert = debruijnIndex []

{-|
  Build De Bruijn indices using a "stack" array
  Insert each name from the lambda expression at the beginning of the stack
  Get the index of that element and make it a var name

  TODO: Replace defaulting to -1 with "Either" structure
|-}
debruijnIndex :: [String] -> Lambda -> DeBruijnLambda
debruijnIndex nameStack e = case e of
  (Var name) -> case elemIndex name nameStack of
    Just n  -> DVar n
    Nothing -> undefined
  (Literal l) -> DLiteral l
  Nil         -> DNil
  (Cons el1 el2) ->
    DCons (debruijnIndex nameStack el1) (debruijnIndex nameStack el2)
  (App e1 e2) -> DApp (debruijnIndex nameStack e1) (debruijnIndex nameStack e2)
  (Add e1 e2) -> DAdd (debruijnIndex nameStack e1) (debruijnIndex nameStack e2)
  (Mul e1 e2) -> DMul (debruijnIndex nameStack e1) (debruijnIndex nameStack e2)
  (LEq e1 e2) -> DLEq (debruijnIndex nameStack e1) (debruijnIndex nameStack e2)
  (If cond ifExp elseExp) -> DIf (debruijnIndex nameStack cond)
                                 (debruijnIndex nameStack ifExp)
                                 (debruijnIndex nameStack elseExp)
  (Case cond ifExp elseExp) -> DCase (debruijnIndex nameStack cond)
                                     (debruijnIndex nameStack ifExp)
                                     (debruijnIndex nameStack elseExp)
  (Abs name expr) -> DAbs (debruijnIndex (name : nameStack) expr)
