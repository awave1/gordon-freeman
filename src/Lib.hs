module Lib
    ( runSECD
    )
where

import           System.Environment

import           Data.Either
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T
import           Control.Monad.State.Lazy


-- import           Pretty
import           DeBruijnConversion
import qualified LambdaSyntax                  as S
import           SECD
import           TypeInference
import           Type

typeInference :: IO ()
typeInference = do
    let identity = S.Abs "x" (S.Var "x")
    let idRes    = getTypeEquation identity
    case idRes of
        Right eq -> do
            print eq
            print $ getTypeVarsInEquation eq
        Left err -> putStrLn err

    let app    = S.App identity identity
    let appRes = getTypeEquation app
    case appRes of
        Right eq -> do
            print eq
            print $ getTypeVarsInEquation eq
        Left err -> putStrLn err


runAll :: [S.Lambda] -> IO ()
runAll []         = putStrLn "done!"
runAll (e : exps) = do
    print e
    putStrLn $ "original: " ++ show e
    putStr "De Bruijn: "
    let deBruijn = debruijnConvert e
    (T.putStrLn . T.pack . show) deBruijn
    let secdCode = compile deBruijn
    putStr "SECD: "
    print secdCode
    putStrLn "Computed SEDC:"
    let (_, _, result) = compute secdCode
    print result
    putStrLn "----------------------------"

    runAll exps

runSECD :: IO ()
runSECD = do
    let add = S.App (S.Abs "x" (S.Add (S.Var "x") (S.Literal (S.LInt 1))))
                    (S.Literal (S.LInt 2))

    let mul = S.App (S.Abs "x" (S.Mul (S.Var "x") (S.Literal (S.LInt 10))))
                    (S.Literal (S.LInt 2))

    let conditionalIf = S.App
            (S.If (S.LEq (S.Literal (S.LInt 1)) (S.Literal (S.LInt 2)))
                  (S.Abs "x" (S.Add (S.Var "x") (S.Literal (S.LInt 1))))
                  (S.Abs "x" (S.Add (S.Var "x") (S.Literal (S.LInt 2))))
            )
            (S.Literal (S.LInt 2))

    let caseStatement = S.App
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

    -- λf.(λa.f(λx.a a x)) (λa.f(λx.a a x))
    let
        combinator = S.Abs
            "f"
            (S.App
                (S.Abs
                    "a"
                    (S.App
                        (S.Var "f")
                        (S.Abs
                            "x"
                            (S.App (S.App (S.Var "a") (S.Var "a")) (S.Var "x"))
                        )
                    )
                )
                (S.Abs
                    "a"
                    (S.App
                        (S.Var "f")
                        (S.Abs
                            "x"
                            (S.App (S.App (S.Var "a") (S.Var "a")) (S.Var "x"))
                        )
                    )
                )
            )

    let
        factorial = S.Abs
            "f"
            (S.Abs
                "n"
                (S.If
                    (S.GEq (S.Var "n") (S.Literal (S.LInt 1)))
                    (S.Mul
                        (S.Var "n")
                        (S.App (S.Var "f")
                               (S.Add (S.Var "n") (S.Literal (S.LInt (-1))))
                        )
                    )
                    (S.Literal (S.LInt 1))
                )
            )

    let fact const =
            S.App (S.App combinator factorial) (S.Literal (S.LInt const))

    runAll [add, mul, conditionalIf, caseStatement, fact 1, fact 2, fact 3]
