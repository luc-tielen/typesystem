
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Main ( main ) where

import Protolude hiding ( Type, TypeError, check )
import qualified Data.Map as Map


type Var = Text

data Expr
  = I Int
  | B Bool
  | V Var
  | Lam Var Expr  -- TODO Lam Expr Expr?
  | App Expr Expr
  | Ann Expr Type
  deriving (Eq, Show)

instance Num Expr where
  fromInteger = I . fromInteger
  e1 + e2 = App (App (V "+") e1) e2
  e1 * e2 = App (App (V "*") e1) e2
  abs = undefined
  signum = undefined
  negate = undefined

data Type
  = TInt
  | TBool
  | TArrow Type Type
  deriving (Eq, Show)

data TypeError
  = TypeMismatch Expr Type Type
  | ExpectedArrowType Expr Type
  | UnboundVariable Var
  | NotSupported
  | WhileChecking Type Expr TypeError
  | WhileInferring Expr TypeError
  deriving (Eq, Show)

data TypedExpr
  = TypedExpr
  { expr :: Expr
  , typeOf :: Type
  } deriving (Eq, Show)

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
        _ <- check t2 e2
        pure $ TypedExpr e1 t  -- TODO recursively annotate!
      _ -> throwError $ ExpectedArrowType e1 expectedType
  e -> do
    typedExpr <- infer e
    let actualType = typeOf typedExpr
    if expectedType == actualType
      then pure $ TypedExpr e expectedType
      else throwError $ TypeMismatch e expectedType actualType

infer :: Expr -> TypeCheckM TypedExpr
infer e = addContext (WhileInferring e) (infer' e)

infer' :: Expr -> TypeCheckM TypedExpr
infer' = \case
  i@(I _) -> pure $ TypedExpr i TInt
  b@(B _) -> pure $ TypedExpr b TBool
  v@(V var) -> asks (Map.lookup var) >>= \case
    Just ty -> pure $ TypedExpr v ty
    Nothing -> throwError $ UnboundVariable var
  e@(App e1 e2) -> do
    e1' <- infer e1
    case typeOf e1' of
      TArrow t1 t2 -> do
        _ <- check t1 e2
        pure $ TypedExpr e t2  -- TODO recursively annotate!
      t -> throwError $ ExpectedArrowType e1 t
  Ann e ty -> check ty e
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
  WhileChecking t e err ->
    "While checking the expression (" <> show e <>
      ") to have type (" <> show t <> "):\n" <> formatErr err
  WhileInferring e err ->
    "While inferring the expression: " <> show e <> "\n" <> formatErr err
  NotSupported -> "Unsupported path in typechecker."


-- Testing code:

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
        , I 20 + I 22
        , V "a", V "b", V "c"
        , Ann (B True) TInt
        , Ann (B True) TBool
        , Ann (I 123) TInt
        , Ann (I 123) TBool
        , Ann (Lam "v" $ B False) (TArrow TInt TBool)
        , Ann (Lam "v" $ B False) (TArrow TInt TInt)
        , Lam "v" $ B False
        , Lam "v" $ V "v"
        , App (B True) (I 0)
        , App (Ann (Lam "v" $ B False) (TArrow TInt TInt)) (I 0)
        , App (Ann (Lam "v" $ B False) (TArrow TInt TBool)) (I 0)
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
main = do
  print ""
  for_ scenarios $ \(e, t) ->
    case runTC typeEnv $ check t e of
      Left err -> putStrLn $ formatErr err
      Right typedExpr -> putStrLn ("Success: " <> show typedExpr :: Text)
