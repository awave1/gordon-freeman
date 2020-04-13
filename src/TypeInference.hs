module TypeInference where

import           Type
import qualified LambdaSyntax                  as L
import           Control.Monad.State.Lazy
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map



-- 1. var names mapped to its type
-- 2. counter
-- 3. equations found

type Context = ([(String, Type)], Int, Int)

{-|
  TODO
|-}
buildTypeEquation :: L.Lambda -> State Context (Either TypeEq String)
buildTypeEquation expr = case expr of
  L.Var x -> do
    (context, _, q) <- get

    case lookup x context of
      Just p  -> return $ Left $ Equation (TVar (show q), p)
      Nothing -> return $ Right $ "Undeclared variable: " ++ x

  L.Abs s t -> do
    (context, n, q) <- get
    let x = succ n
    let y = succ x

    put ((s, TVar $ show x) : context, succ y, y)

    e <- buildTypeEquation t
    case e of
      Left e' -> do
        let typeEq = Exists
              [show x, show y]
              [ Equation (TVar $ show q, TFun (TVar $ show x) (TVar $ show y))
              , e'
              ]
        return $ Left typeEq
      Right err -> return $ Right $ "error (abs): " ++ err

  L.App e1 e2 -> do
    (context, n, q) <- get
    let l = n + 1
    let r = n + 2
    put (context, n + 3, l)

    e1'                <- buildTypeEquation e1
    (context', x', q') <- get
    put (context, x', r)

    e2' <- buildTypeEquation e2
    case (e1', e2') of
      (Left te1, Left te2) -> do
        let eq = Exists
              [show l, show r]
              [ Equation (TVar (show r), TFun (TVar $ show l) (TVar (show q)))
              , te1
              , te2
              ]

        return $ Left eq
      (Right err, _        ) -> return $ Right $ "error (app): " ++ err
      (_        , Right err) -> return $ Right $ "error (app): " ++ err

  L.Fix expr -> do
    (context, z, q) <- get
    put (context, z + 2, q + 1)

    e <- buildTypeEquation expr
    case e of
      Left e' -> do
        let typeEq = Exists
              [show z]
              [ Equation (TVar $ show z, TFun (TVar $ show q) (TVar (show q)))
              , e'
              ]

        return $ Left typeEq
      Right err -> return $ Right $ "error (fix): " ++ err

  L.Literal lit -> do
    (_, _, q) <- get
    case lit of
      L.LInt  _ -> return $ Left $ Equation (TVar $ show q, TInt)
      L.LBool _ -> return $ Left $ Equation (TVar $ show q, TBool)

getType :: L.Lambda -> Either TypeEq String
getType e = evalState (buildTypeEquation e) ([], 0, 0)

{-|
  TODO
|-}
inferType :: ()
inferType = undefined
