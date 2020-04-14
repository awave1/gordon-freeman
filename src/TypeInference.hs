module TypeInference where

import           Type
import           Pretty
import qualified LambdaSyntax                  as L
import           Control.Monad.State.Lazy
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T



-- 1. var names mapped to its type
-- 2. counter
-- 3. equations found

-- type Context = ([(String, Type)], Int, Int)
type Context = ([(String, Type)], Int, Int)

{-|
  TODO
|-}
buildTypeEquation :: L.Lambda -> State Context (Either String TypeEq)
buildTypeEquation expr = case expr of
  L.Var x -> do
    (context, _, q) <- get

    case lookup x context of
      Just p  -> return $ Right $ Equation (TVar (show q), p)
      Nothing -> return $ Left $ "Undeclared variable: " ++ x

  L.Abs s t -> do
    (context, n, q) <- get
    let x = succ n
    let y = succ x

    put ((s, TVar $ show x) : context, succ y, y)

    e <- buildTypeEquation t
    case e of
      Right e' -> do
        let typeEq = Exists
              [show x, show y]
              [ Equation (TVar $ show q, TFun (TVar $ show x) (TVar $ show y))
              , e'
              ]
        return $ Right typeEq
      Left err -> return $ Left $ "error (abs): " ++ err

  L.App e1 e2 -> do
    (context, n, q) <- get
    let l = succ n
    let r = succ l
    put (context, succ r, l)

    e1'                <- buildTypeEquation e1
    (context', x', q') <- get
    put (context, x', r)

    e2' <- buildTypeEquation e2
    case (e1', e2') of
      (Right te1, Right te2) -> do
        let eq = Exists
              [show l, show r]
              [ Equation (TVar (show r), TFun (TVar $ show l) (TVar (show q)))
              , te1
              , te2
              ]

        return $ Right eq
      (Left err, _       ) -> return $ Left $ "error (app): " ++ err
      (_       , Left err) -> return $ Left $ "error (app): " ++ err

  L.Fix expr -> do
    (context, z, q) <- get
    put (context, (succ . succ) z, succ q)

    e <- buildTypeEquation expr
    case e of
      Right e' -> do
        let typeEq = Exists
              [show z]
              [ Equation (TVar $ show z, TFun (TVar $ show q) (TVar (show q)))
              , e'
              ]

        return $ Right typeEq
      Left err -> return $ Left $ "error (fix): " ++ err

  L.Literal lit -> do
    (_, _, q) <- get
    case lit of
      L.LInt  _ -> return $ Right $ Equation (TVar $ show q, TInt)
      L.LBool _ -> return $ Right $ Equation (TVar $ show q, TBool)

getTypeEquation :: L.Lambda -> Either String TypeEq
getTypeEquation e = evalState (buildTypeEquation e) ([], 0, 0)

getTypeVars :: Type -> [String]
getTypeVars t = case t of
  TVar v   -> [v]
  TFun v f -> getTypeVars v ++ getTypeVars f

getTypeVarsInEquation :: TypeEq -> [String]
getTypeVarsInEquation eq = case eq of
  Equation (_, t2) -> getTypeVars t2
  Exists _ eqs     -> concatMap getTypeVarsInEquation eqs

{-|
  TODO: docs
|-}
check :: String -> Type -> Either String [(String, Type)]
check x t
  | t == TVar x = Left []
  | x `elem` getTypeVars t = Left
    ("occurs check failed: " ++ x ++ " = " ++ show t)
  | otherwise = Right [(x, t)]

unify :: Type -> Type -> [(String, Type)] -> Either String [(String, Type)]
unify t1 t2 result = case (t1, t2) of
  (TVar v, anyType) -> case check v anyType of
    Right sub -> Right $ sub ++ result
    Left  err -> Left err

  (anyType, TVar v) -> case check v anyType of
    Right sub -> Right $ sub ++ result
    Left  err -> Left err

  (TFun v1 f1, TFun v2 f2) -> unify v1 v2 result >>= unify f1 f2 -- "Sequentially compose two actions, passing any value produced by the first as an argument to the second."

solveTypeEquation :: TypeEq -> Either String Type
solveTypeEquation typeEq = case typeEq of
  Equation (left, right) -> case (left, right) of
    (TVar l, TVar r) -> undefined
  Exists vars equations -> undefined

{-|
  TODO
|-}
infer :: L.Lambda -> Either String Type
infer expr = case getTypeEquation expr of
  Right eq -> case solveTypeEquation eq of
    Right inferredType -> Right inferredType
    Left err ->
      Left
        $  "failed to solve type equation `"
        ++ show eq
        ++ "` for expression `"
        ++ show expr
        ++ "`.\nreason: "
        ++ err
  Left err ->
    Left
      $  "type inference failed for expression `"
      ++ show expr
      ++ "`.\nreason: "
      ++ err
