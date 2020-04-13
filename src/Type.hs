module Type where

import           Data.List

data Type = TVar String
          | TFun Type Type
          | TProd Type Type
          | TList Type Type
          | TNat
          | TInt
          | TBool
          | TUnit
          | TConst String
          deriving (Eq)

instance Show Type where
  show (TVar v       ) = v
  show (TFun var body) = show var ++ " -> " ++ show body
  show TInt            = "Int"
  show TBool           = "Bool"
  show TNat            = "Nat"
  show (TConst s)      = s

data TypeEq = Equation (Type, Type)
            | Exists [String] [TypeEq]
            deriving (Eq)

instance Show TypeEq where
  show (Equation (t1, t2)    ) = show t1 ++ " = " ++ show t2
  show (Exists vars equations) = "∃" ++ vs ++ "." ++ "(" ++ eqs ++ ")"
   where
    vs  = intercalate "," vars
    eqs = intercalate ", " (map show equations)
