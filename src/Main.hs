
module Main ( main ) where

import Protolude hiding ( Type, TypeError, check )
import qualified Data.Map as Map
import qualified Data.Text as T


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

typeOf :: TypedExpr -> Type
typeOf = \case
  I' ty _ -> ty
  B' ty _ -> ty
  V' ty _ -> ty
  If' ty _ _ _ -> ty
  Lam' ty _ _ -> ty
  App' ty _ _ -> ty
  TyAnn' _ ty -> ty

data Type
  = TInt
  | TBool
  | TArrow Type Type
  deriving (Eq, Show)

data TypeError
  = TypeMismatch Expr Type Type
  | ExpectedArrowType Expr Type
  | UnboundVariable Var
  | FoundHole TypeEnv
  | NotSupported
  | WhileChecking Type Expr TypeError
  | WhileInferring Expr TypeError
  deriving (Eq, Show)


type TypeEnv = Map Var Type

type TypeCheckM = ReaderT TypeEnv (Except TypeError)


runTC :: TypeEnv -> TypeCheckM a -> Either TypeError a
runTC env m = runExcept $ runReaderT m env

addContext :: (TypeError -> TypeError) -> TypeCheckM a -> TypeCheckM a
addContext ctx m = catchError m (throwError . ctx)

check :: Type -> Expr -> TypeCheckM TypedExpr
check t e = addContext (WhileChecking t e) (check' t e)

check' :: Type -> Expr -> TypeCheckM TypedExpr
check' expectedType = \case
  e1@(Lam v e2) ->
    case expectedType of
      t@(TArrow t1 t2) -> local (Map.insert v t1) $ do
        e2' <- check t2 e2
        pure $ Lam' t v e2'
      _ -> throwError $ ExpectedArrowType e1 expectedType
  If c t f -> do
    c' <- check TBool c
    t' <- check expectedType t
    f' <- check expectedType f
    pure $ If' expectedType c' t' f'
  Hole -> do
    env <- ask
    throwError $ FoundHole env
  e -> do
    typedExpr <- infer e
    let actualType = typeOf typedExpr
    if expectedType == actualType
      then pure typedExpr
      else throwError $ TypeMismatch e expectedType actualType

infer :: Expr -> TypeCheckM TypedExpr
infer e = addContext (WhileInferring e) (infer' e)

infer' :: Expr -> TypeCheckM TypedExpr
infer' = \case
  I i -> pure $ I' TInt i
  B b -> pure $ B' TBool b
  V var -> asks (Map.lookup var) >>= \case
    Just ty -> pure $ V' ty var
    Nothing -> throwError $ UnboundVariable var
  App e1 e2 -> do
    e1' <- infer e1
    case typeOf e1' of
      TArrow t1 t2 -> do
        e2' <- check t1 e2
        pure $ App' t2 e1' e2'
      t -> throwError $ ExpectedArrowType e1 t
  TyAnn e ty -> check ty e
  Hole -> do
    env <- ask
    throwError $ FoundHole env
  _ -> throwError NotSupported

formatErr :: TypeError -> Text
formatErr = \case
  TypeMismatch _ expectedType actualType ->
    "Type mismatch!\n   Expected type: " <> show expectedType <> "\n"
      <> "     Actual type: " <> show actualType
  ExpectedArrowType _ t ->
    "Expected type " <> show t <> ", but got function type instead."
  UnboundVariable v ->
    "Found unbound variable: " <> v <> "."
  FoundHole env ->
    "Found hole, environment:\n" <> formatTypeEnv env
  WhileChecking t e err ->
    "While checking the expression (" <> show e <>
      ") to have type (" <> show t <> "):\n" <> formatErr err
  WhileInferring e err ->
    "While inferring the expression: " <> show e <> "\n" <> formatErr err
  NotSupported -> "Unsupported path in typechecker."

formatTypeEnv :: TypeEnv -> Text
formatTypeEnv env =
  let entries = Map.toList env
      toLine (var, ty) = "- " <> show var <> ": " <> show ty
   in T.pack $ intercalate "\n" $ map toLine entries


-- Testing code:

-- Only used for more easily constructing expressions
instance Num Expr where
  fromInteger = I . fromInteger
  e1 + e2 = App (App (V "+") e1) e2
  e1 * e2 = App (App (V "*") e1) e2
  abs = App (V "abs")
  signum = App (V "signum")
  negate = App (V "negate")


typeEnv :: TypeEnv
typeEnv = Map.fromList
  [ ("a", TInt)
  , ("b", TBool)
  , ("+", TArrow TInt (TArrow TInt TInt))
  ]

scenarios :: [(Expr, Type)]
scenarios =
  let exprs =
        [ 0
        , 1
        , B True
        , B False
        , B True + B False
        , 20 + 22
        , V "a", V "b", V "c"
        , If 0 1 2
        , If (B True) 1 2
        , If (B True) (B False) 2
        , If (B True) 1 (B False)
        , TyAnn (B True) TInt
        , TyAnn (B True) TBool
        , TyAnn 123 TInt
        , TyAnn 123 TBool
        , TyAnn (Lam "v" $ B False) (TArrow TInt TBool)
        , TyAnn (Lam "v" $ B False) (TArrow TInt TInt)
        , Lam "v" $ B False
        , Lam "v" $ V "v"
        , App (B True) 0
        , App (TyAnn (Lam "v" $ B False) (TArrow TInt TInt)) 0
        , App (TyAnn (Lam "v" $ B False) (TArrow TInt TBool)) 0
        , Hole
        , Lam "v" Hole
        , App Hole 0
        ]
      types =
        [ TBool
        , TInt
        , TArrow TInt TBool
        , TArrow TBool TInt
        , TArrow TInt TInt
        ]
    in [(e, ty) | e <- exprs, ty <- types]

main :: IO ()
main = for_ scenarios $ \(e, t) ->
  case runTC typeEnv $ check t e of
    Left err -> putStrLn $ formatErr err
    Right typedExpr -> putStrLn ("Success: " <> show typedExpr :: Text)
