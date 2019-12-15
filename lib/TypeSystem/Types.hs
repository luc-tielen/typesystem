
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

newtype SkolemScope = SkolemScope Int
  deriving (Eq, Ord, Show)

newtype Skolem = Skolem Int
  deriving (Eq, Show)

data Type
  = TInt
  | TBool
  | TVar Var                        -- Type variable, provided by the user
  | TUnknown Int                    -- Used only during unification, when having to guess a type
  | TSkolem Var SkolemScope Skolem  -- Rigid type variable, only available in a specific scope
  | TForAll Var (Maybe SkolemScope) Type
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
  | EscapedSkolem Var Type
  | MultipleErrors (NonEmpty TypeError)
  | WhileChecking Type Expr TypeError
  | WhileInferring Expr TypeError
  | WhileUnifyingTypes Type Type TypeError
  | NotSupported
  deriving (Eq, Show)

