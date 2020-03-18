module DeBruijnConversion
  ( debruijnConvert
  )
where

import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           Data.Set                       ( Set )
import qualified Data.Set                      as Set
import           Data.List
import           Data.Maybe

import           Syntax

{-|
  Perform De Bruijn conversion
|-}
debruijnConvert :: Expr -> Expr
debruijnConvert = debruijnIndex []

{-|
  Build De Bruijn indices using a "stack" array
  Insert each name from the lambda expression at the beginning of the stack
  Get the index of that element and make it a var name

  TODO: Replace defaulting to -1 with "Either" structure
|-}
debruijnIndex :: [String] -> Expr -> Expr
debruijnIndex nameStack e = case e of
  (Var name) -> Var (show $ fromMaybe (-1) (elemIndex name nameStack))
  (App e1 e2) -> App (debruijnIndex nameStack e1) (debruijnIndex nameStack e2)
  (Lam name expr) -> Lam "" (debruijnIndex (name : nameStack) expr)
