module Lib
    ( runSECD
    )
where

import           System.Environment

import           Data.Either
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T


import           Parser
import           Pretty
import           DeBruijnConversion
import qualified LambdaSyntax                  as S
import           SECD

{-
    - Turning (extended) lambda terms into De Bruijn notation
    - Compiling De Bruijn terms into CES machine code (see attached document)
    - Writing the CES machine: its  step function and running code on the machine.   (You may want a debugging mode!)
    - Writing five example programs (including some recursive ones! e.g factorial) in the extended lambda calculus.
-}

runAll :: [S.Lambda] -> IO ()
runAll []         = putStrLn "done!"
runAll (e : exps) = do
    print e
    putStrLn $ "original: " ++ pretty e
    putStr "De Bruijn: "
    let deBruijn = debruijnConvert e
    (T.putStrLn . T.pack . prettyDeBruijn) deBruijn
    let secdCode = compile deBruijn
    putStr "SECD: "
    print secdCode
    putStrLn "Computed SEDC:"
    print $ compute secdCode
    putStrLn "----------------------------"

    runAll exps

run' :: [String] -> IO ()
run' args
    | not $ null args = do
        let fun = head args
        case parseExpr fun of
            Right parsed -> runAll [parsed]
            Left  err    -> print err
    | otherwise = do
        let add = S.App
                (S.Abs "x" (S.Add (S.Var "x") (S.Literal (S.LInt 1))))
                (S.Literal (S.LInt 2))

        let cond = S.App
                (S.If (S.LEq (S.Literal (S.LInt 1)) (S.Literal (S.LInt 2)))
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

        -- it's possible to run the omega,
        -- but as you'd expect, it'll run indefinetly
        let o = S.Abs "x" (S.App (S.Var "x") (S.Var "x"))
        let omega = S.App o o

        runAll [add, cond, case']

runSECD :: IO ()
runSECD = do
    args <- getArgs
    run' args
