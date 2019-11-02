
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Main ( main ) where

import Protolude hiding ( Type, TypeError, check )
import qualified Data.Map as Map
import qualified Data.Text as T


data Phase = PreTC | PostTC
type PreTC = 'PreTC
type PostTC = 'PostTC

type family Ann (ph :: Phase) where
  Ann PreTC = ()
  Ann PostTC = Type

type Var = Text

data Expr (ph :: Phase)
  = I (Ann ph) Int
  | B (Ann ph) Bool
  | V (Ann ph) Var
  | Lam (Ann ph) Var (Expr ph)
  | App (Expr ph) (Expr ph)
  | TyAnn (Expr ph) Type
  | Hole

typeOf :: Expr PostTC -> Type
typeOf = \case
  I ty _ -> ty
  B ty _ -> ty
  V ty _ -> ty
  Lam ty _ _ -> ty
  App e1 _ ->
    case typeOf e1 of
      TArrow _ t2 -> t2
      _ -> panic "Internal error: invalid type for App constructor."
  TyAnn _ ty -> ty
  Hole -> panic "Internal error: should not call typeOf on a hole."

deriving instance Eq (Ann ph) => Eq (Expr ph)
deriving instance Show (Ann ph) => Show (Expr ph)

instance Num (Expr PreTC) where
  fromInteger = I () . fromInteger
  e1 + e2 = App (App (V () "+") e1) e2
  e1 * e2 = App (App (V () "*") e1) e2
  abs = undefined
  signum = undefined
  negate = undefined

data Type
  = TInt
  | TBool
  | TArrow Type Type
  deriving (Eq, Show)

data TypeError
  = TypeMismatch (Expr PreTC) Type Type
  | ExpectedArrowType (Expr PreTC) Type
  | UnboundVariable Var
  | FoundHole TypeEnv
  | NotSupported
  | WhileChecking Type (Expr PreTC) TypeError
  | WhileInferring (Expr PreTC) TypeError
  deriving (Eq, Show)


type TypeEnv = Map Var Type

type TypeCheckM = ReaderT TypeEnv (Except TypeError)


runTC :: TypeEnv -> TypeCheckM a -> Either TypeError a
runTC env m = runExcept $ runReaderT m env

addContext :: (TypeError -> TypeError) -> TypeCheckM a -> TypeCheckM a
addContext ctx m = catchError m (throwError . ctx)

check :: Type -> Expr PreTC -> TypeCheckM (Expr PostTC)
check t e = addContext (WhileChecking t e) (check' t e)

check' :: Type -> Expr PreTC -> TypeCheckM (Expr PostTC)
check' expectedType = \case
  e1@(Lam _ v e2) ->
    case expectedType of
      t@(TArrow t1 t2) -> local (Map.insert v t1) $ do
        e2' <- check t2 e2
        pure $ Lam t v e2'
      _ -> throwError $ ExpectedArrowType e1 expectedType
  Hole -> do
    env <- ask
    throwError $ FoundHole env
  e -> do
    typedExpr <- infer e
    let actualType = typeOf typedExpr
    if expectedType == actualType
      then pure typedExpr
      else throwError $ TypeMismatch e expectedType actualType

infer :: Expr PreTC -> TypeCheckM (Expr PostTC)
infer e = addContext (WhileInferring e) (infer' e)

infer' :: Expr PreTC -> TypeCheckM (Expr PostTC)
infer' = \case
  (I _ i) -> pure $ I TInt i
  (B _ b) -> pure $ B TBool b
  (V _ var) -> asks (Map.lookup var) >>= \case
    Just ty -> pure $ V ty var
    Nothing -> throwError $ UnboundVariable var
  App e1 e2 -> do
    e1' <- infer e1
    case typeOf e1' of
      TArrow t1 _ -> do
        e2' <- check t1 e2
        pure $ App e1' e2'
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

typeEnv :: TypeEnv
typeEnv = Map.fromList
  [ ("a", TInt)
  , ("b", TBool)
  , ("+", TArrow TInt (TArrow TInt TInt))
  ]

scenarios :: [(Expr PreTC, Type)]
scenarios =
  let exprs =
        [ 0
        , 1
        , B () True
        , B () False
        , B () True + B () False
        , 20 + 22
        , V () "a", V () "b", V () "c"
        , TyAnn (B () True) TInt
        , TyAnn (B () True) TBool
        , TyAnn 123 TInt
        , TyAnn 123 TBool
        , TyAnn (Lam () "v" $ B () False) (TArrow TInt TBool)
        , TyAnn (Lam () "v" $ B () False) (TArrow TInt TInt)
        , Lam () "v" $ B () False
        , Lam () "v" $ V () "v"
        , App (B () True) 0
        , App (TyAnn (Lam () "v" $ B () False) (TArrow TInt TInt)) 0
        , App (TyAnn (Lam () "v" $ B () False) (TArrow TInt TBool)) 0
        , Hole
        , Lam () "v" Hole
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
main = do
  print ""
  for_ scenarios $ \(e, t) ->
    case runTC typeEnv $ check t e of
      Left err -> putStrLn $ formatErr err
      Right typedExpr -> putStrLn ("Success: " <> show typedExpr :: Text)
