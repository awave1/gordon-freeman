module Lib where

import           System.Environment

import           Data.Either
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T


import           Parser
import           Pretty
import           DeBruijnConversion

{-
    - Turning (extended) lambda terms into De Bruijn notation
    - Compiling De Bruijn terms into CES machine code (see attached document)
    - Writing the CES machine: its  step function and running code on the machine.   (You may want a debugging mode!)
    - Writing five example programs (including some recursive ones! e.g factorial) in the extended lambda calculus.
-}

data E
    = EApp E E
    | EAbs E
    | EVar Int
    | EInt Int
    deriving (Show)

type Name = String
data Expr = Var Name
          | App Expr Expr
          | Lambda Name Expr
          deriving (Show)

eval :: E -> E
eval (EApp fun arg) = case eval fun of
    EAbs body -> eval $ sub 0 body      where
        sub n e = case e of
            EApp e1 e2 -> EApp (sub n e1) (sub n e2)
            EAbs e'    -> EAbs $ sub (n + 1) e'
            EVar n' | n == n'   -> arg
                    | -- ^substitute, arg has no free vars
                      otherwise -> EVar n'
            EInt i -> EInt i
    other -> EApp other arg
eval x = x

someFunc :: IO ()
someFunc = do
    args <- getArgs
    let fun = head args
    case parseExpr fun of
        Right parsed -> do
            putStrLn $ "original: " ++ pretty parsed
            putStr "De Bruijn: "
            (T.putStrLn . T.pack . prettyDeBruijn) (debruijnConvert parsed)
        Left err -> print err
