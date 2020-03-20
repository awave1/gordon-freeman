module Lib where

import           System.Environment

import           Data.Either
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T


import           Parser
import           Pretty
import           DeBruijnConversion
import qualified Syntax                        as S
import           SECD

{-
    - Turning (extended) lambda terms into De Bruijn notation
    - Compiling De Bruijn terms into CES machine code (see attached document)
    - Writing the CES machine: its  step function and running code on the machine.   (You may want a debugging mode!)
    - Writing five example programs (including some recursive ones! e.g factorial) in the extended lambda calculus.
-}

someFunc :: IO ()
someFunc = do
    args <- getArgs

    let add = S.App (S.Abs "x" (S.Add (S.Var "x") (S.Literal (S.LInt 1))))
                    (S.Literal (S.LInt 2))

    let cond = S.App
            (S.If (S.LEq (S.Literal (S.LInt 1)) (S.Literal (S.LInt 1)))
                  (S.Abs "x" (S.Add (S.Var "x") (S.Literal (S.LInt 1))))
                  (S.Abs "x" (S.Add (S.Var "x") (S.Literal (S.LInt 2))))
            )
            (S.Literal (S.LInt 2))

    let case' = S.App
            (S.Abs
                "x"
                (S.Case (S.Var "x")
                        (S.Add (S.Var "x") (S.Literal (S.LInt 1)))
                        (S.Add (S.Var "x") (S.Literal (S.LInt 2)))
                )
            )
            (S.Cons (S.Literal (S.LInt 2)) S.Nil)

    print add
    print cond
    print case'

    putStrLn ""
    putStrLn $ "original: " ++ pretty case'
    putStr "De Bruijn: "
    let deBruijn = debruijnConvert case'
    (T.putStrLn . T.pack . prettyDeBruijn) deBruijn
    let secdCode = compile deBruijn
    putStr "SECD: "
    print secdCode

    -- let fun = head args
    -- case parseExpr fun of
    --     Right parsed -> do
    --         print parsed
    --         let e = S.Abs "x" (S.Abs "y" (S.Add (S.Var "x") (S.Var "y")))
    --         putStrLn $ "original: " ++ pretty parsed
    --         putStr "De Bruijn: "
    --         let deBruijn = debruijnConvert parsed
    --         (T.putStrLn . T.pack . prettyDeBruijn) (debruijnConvert e)
    --     Left err -> print err
