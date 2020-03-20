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
import qualified Syntax                        as S
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
    putStrLn "----------------------------"

    runAll exps

runSECD :: IO ()
runSECD = do
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

    let o = S.Abs "x" (S.App (S.Var "x") (S.Var "x"))
    let omega = S.App o o

    runAll [add, cond, case', omega]

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
