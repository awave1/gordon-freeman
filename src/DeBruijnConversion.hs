module DeBruijnConversion
  ( debruijnConvert
  )
where

import           Data.List
import           Data.Maybe

import           Syntax

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
  (Var name) -> DVar (fromMaybe (-1) (elemIndex name nameStack))
  (Literal l) -> DLiteral l
  (Add e1 e2) -> DAdd (debruijnIndex nameStack e1) (debruijnIndex nameStack e2)
  (App e1 e2) -> DApp (debruijnIndex nameStack e1) (debruijnIndex nameStack e2)
  (Abs name expr) -> DAbs (debruijnIndex (name : nameStack) expr)
