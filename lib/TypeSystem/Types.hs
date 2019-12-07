
module TypeSystem.Types
  ( module TypeSystem.Types
  ) where

import Protolude hiding ( Type, TypeError )

type Var = Text

data Expr
  = I Int
  | B Bool
  | V Var
  | If Expr Expr Expr
  | Lam Var Expr
  | App Expr Expr
  | TyAnn Expr Type
  | Hole
  deriving (Eq, Show)

data TypedExpr
  = I' Type Int
  | B' Type Bool
  | V' Type Var
  | If' Type TypedExpr TypedExpr TypedExpr
  | Lam' Type Var TypedExpr
  | App' Type TypedExpr TypedExpr
  | TyAnn' TypedExpr Type
  deriving (Eq, Show)

data Type
  = TInt
  | TBool
  | TVar Text     -- Type variable, provided by the user
  | TUnknown Int  -- Used only during unification, when having to guess a type
  | TArrow Type Type
  deriving (Eq, Show)

type TypeEnv = Map Var Type

data TypeError
  = TypeMismatch Type Type
  | ExpectedArrowType Expr Type
  | UnboundVariable Var
  | FoundHole TypeEnv
  | OccursCheck Type Type
  | UnificationFailure Type Type
  | WhileChecking Type Expr TypeError
  | WhileInferring Expr TypeError
  | WhileUnifyingTypes Type Type TypeError
  | NotSupported
  deriving (Eq, Show)

