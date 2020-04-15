module TypeInference where

import           Type
import           Pretty
import qualified LambdaSyntax                  as L

import           Control.Monad.State.Lazy

import           Data.Foldable                  ( toList )
import           Data.Set                       ( Set )
import qualified Data.Set                      as Set
import           Data.Either
import           Data.List
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T

type Context = ([(String, Type)], Int, Int)

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

  L.Zero -> do
    (_, _, q) <- get
    return $ Right $ Equation (TVar $ show q, TNat)

getTypeEquation :: L.Lambda -> Either String TypeEq
getTypeEquation e = evalState (buildTypeEquation e) ([], 0, 0)

getTypeEquationUnsafe :: L.Lambda -> TypeEq
getTypeEquationUnsafe e =
  fromRight (Exists [] []) $ evalState (buildTypeEquation e) ([], 0, 0)

getTypeVarsInEquation :: TypeEq -> [String]
getTypeVarsInEquation eq = case eq of
  Equation (_, t2) -> getTypeVars t2
  Exists _ eqs     -> concatMap getTypeVarsInEquation eqs
 where
  -- getTypeVars :: Type -> [String]
  getTypeVars t = case t of
    TVar v   -> [v]
    TFun v f -> getTypeVars v ++ getTypeVars f
    _        -> [show t]

getBoundVars :: TypeEq -> [String]
getBoundVars eq = case eq of
  Equation (TVar s, _) -> [s]
  Exists _ eqs         -> concatMap getBoundVars eqs

flattenEq :: TypeEq -> TypeEq
flattenEq eq = Exists (getExistentialVars eq) (getAllEquations eq)

getExistentialVars :: TypeEq -> [String]
getExistentialVars eq = case eq of
  Exists vars eqs -> vars ++ concatMap getExistentialVars eqs
  Equation t      -> []

getAllEquations :: TypeEq -> [TypeEq]
getAllEquations eq = case eq of
  Exists vars eqs -> concatMap getAllEquations eqs
  Equation e      -> [Equation e]

{-|
  get variables that can be eliminated
|-}
getEliminatable :: TypeEq -> [String]
getEliminatable typeEq =
  getExistentialVars typeEq `intersect` getBoundVars typeEq

{-
  Builds a map with variables that can be eliminated
-}
buildEliminatableMap :: TypeEq -> Map String [Type]
buildEliminatableMap typeEq = combineBinded' (getAllEquations typeEq)
                                             (initMap typeEq)
 where
  -- initMap :: TypeEq -> Map String [a]
  initMap typeEq = Map.fromList $ zip
    (getEliminatable typeEq)
    (replicate (length $ getEliminatable typeEq) [])

  -- combineBinded' :: [TypeEq] -> Map String [TypeEq] -> Map String [TypeEq]
  combineBinded' [] resMap = resMap
  combineBinded' (Equation (TVar leftV, r) : equations) resMap =
    case resMap Map.!? leftV of
      Just list ->
        combineBinded' equations $ Map.insert leftV (r : list) resMap
      Nothing -> combineBinded' equations resMap

{-|
  Replace variables in equation
|-}
replaceInEquations :: [TypeEq] -> Map String [Type] -> [TypeEq]
replaceInEquations []         _ = []
replaceInEquations (eq : eqs) m = case eq of
  Equation (left, right) ->
    Equation (left, replaceType right m) : replaceInEquations eqs m

{-|
  Replaces a variable in a type
|-}
replaceType :: Type -> Map String [Type] -> Type
replaceType t m = case t of
  TVar v -> case m Map.!? v of
    Just ts -> head ts
    Nothing -> TVar v
  TFun v f ->
    TFun (replaceType v $ replaceInMap m) (replaceType f $ replaceInMap m)
  _ -> t
 where
  -- replaceInMap :: Map String [Type] -> Map String [Type]
  replaceInMap m = replaceInMap' m (Map.keys m)
  replaceInMap' m []         = m
  replaceInMap' m (k : keys) = do
    let newMap = Map.insert k (map (`replaceType` m) $ m Map.! k) m
    replaceInMap' newMap keys


{-|
  Solves the type equation
|-}
solve :: TypeEq -> TypeEq
solve typeEq = Exists (vars \\ getEliminatable flattenedTypeEq)
                      (replaceInEquations equations eliminatableMap)
 where
  flattenedTypeEq         = flattenEq typeEq
  (Exists vars equations) = flattenedTypeEq
  eliminatableMap         = buildEliminatableMap flattenedTypeEq

{-|
  TODO
|-}
infer :: L.Lambda -> Either String Type
infer expr = case getTypeEquation expr of
  Right eq -> case solve eq of
    Exists _ (Equation (_, t) : rest) -> Right t
  Left err ->
    Left
      $  "type inference failed for expression `"
      ++ show expr
      ++ "`.\nreason: "
      ++ err
