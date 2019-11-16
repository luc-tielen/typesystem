
module Main ( main ) where

import Protolude hiding ( Type, TypeError, check )
import Control.Monad.RWS
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.IntMap as IntMap
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
  | TVar Text     -- Type variable, provided by the user
  | TUnknown Int  -- Used only during unification, when having to guess a type
  | TArrow Type Type
  deriving (Eq, Show)

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


type TypeEnv = Map Var Type

type Substitution = IntMap Type

data CheckState = CheckState { nextVar :: Int, substitution :: Substitution }
  deriving Show

type TypeCheckM = ExceptT TypeError (RWS TypeEnv () CheckState)


runTC :: TypeEnv -> TypeCheckM a -> (Substitution, Either TypeError a)
runTC env m =
  let (res, cs, _) = runRWS (runExceptT m) env (CheckState 0 mempty)
      subst = substitution cs
   in (subst, res)

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
    unifyType expectedType actualType
    pure typedExpr


infer :: Expr -> TypeCheckM TypedExpr
infer e = addContext (WhileInferring e) (infer' e)

infer' :: Expr -> TypeCheckM TypedExpr
infer' = \case
  I i -> pure $ I' TInt i
  B b -> pure $ B' TBool b
  V var -> asks (Map.lookup var) >>= \case
    Just ty -> pure $ V' ty var
    Nothing -> throwError $ UnboundVariable var
  Lam v e -> do
    ty <- freshType
    local (Map.insert v ty) $ do
      e' <- infer e
      let lamType = TArrow ty (typeOf e')
      pure $ Lam' lamType v e'
  App e1 e2 -> do
    e1' <- infer e1
    subst <- gets substitution
    let t = typeOf e1'
    case substituteType subst t of
      TArrow t1 t2 -> do
        e2' <- check t1 e2
        pure $ App' t2 e1' e2'
      ty -> throwError $ ExpectedArrowType e1 ty
  TyAnn e ty -> check ty e
  If c t e -> do
    c' <- check TBool c
    t' <- infer t
    e' <- infer e
    let tyT = typeOf t'
        tyE = typeOf e'
    unifyType tyT tyE
    pure $ If' tyT c' t' e'
  Hole -> do
    env <- ask
    throwError $ FoundHole env

-- Generate a fresh unification variable
freshType :: TypeCheckM Type
freshType = do
  ty <- TUnknown <$> gets nextVar
  modify $ \s -> s { nextVar = nextVar s + 1 }
  pure ty

-- Tries to unify 2 types, updating the Substitution along the way.
unifyType :: Type -> Type -> TypeCheckM ()
unifyType t1 t2 = addContext (WhileUnifyingTypes t1 t2) $ do
  subst <- gets substitution
  unifyType' (substituteType subst t1) (substituteType subst t2)
  where
    unifyType' (TArrow t11 t12) (TArrow t21 t22) = do
      unifyType t11 t21
      unifyType t12 t22
    unifyType' TInt TInt = pure ()
    unifyType' TBool TBool = pure ()
    unifyType' (TVar v1) (TVar v2) | v1 == v2 = pure ()
    unifyType' (TUnknown u1) (TUnknown u2) | u1 == u2 = pure ()
    unifyType' (TUnknown u) t = solveType u t
    unifyType' t (TUnknown u) = solveType u t
    unifyType' ty1 ty2 = throwError $ UnificationFailure ty1 ty2

-- Recursively substitute the type given a subtitution
substituteType :: Substitution -> Type -> Type
substituteType subst = \case
  TArrow t1 t2 -> TArrow (substituteType subst t1) (substituteType subst t2)
  TUnknown unknown ->
    case IntMap.lookup unknown subst of
      Nothing -> TUnknown unknown
      Just (TUnknown unknown') | unknown == unknown' -> TUnknown unknown'
      Just t -> substituteType subst t
  t -> t

-- Solve the current type and update the Substitution
solveType :: Int -> Type -> TypeCheckM ()
solveType u t = do
  occursCheck u t
  modify $ \s ->
    let subst = substitution s
     in s { substitution = IntMap.insert u t subst }

occursCheck :: Int -> Type -> TypeCheckM ()
occursCheck unknown ty =
  when (isJust $ List.find (== unknown) (unknowns ty)) $
    throwError $ OccursCheck (TUnknown unknown) ty
  where unknowns = \case
          TUnknown u -> [u]
          TArrow t1 t2 -> unknowns t1 <> unknowns t2
          _ -> mempty

formatErr :: TypeError -> Text
formatErr = \case
  TypeMismatch expectedType actualType ->
    "Type mismatch!\n   Expected type: " <> show expectedType <> "\n"
      <> "     Actual type: " <> show actualType
  ExpectedArrowType _ t ->
    "Expected type " <> show t <> ", but got function type instead."
  UnboundVariable v ->
    "Found unbound variable: " <> v <> "."
  FoundHole env ->
    "Found hole, environment:\n" <> formatTypeEnv env
  OccursCheck t1 t2 ->
    "Occurs check: cannot construct the infinite type: " <> show t1 <> " ~ " <> show t2
  UnificationFailure t1 t2 ->
    "Failed to unify the 2 following types:\n- " <> show t1 <> "\n- " <> show t2
  WhileChecking t e err ->
    "While checking the expression (" <> show e <>
      ") to have type (" <> show t <> "):\n" <> formatErr err
  WhileInferring e err ->
    "While inferring the expression: " <> show e <> "\n" <> formatErr err
  WhileUnifyingTypes t1 t2 err ->
    "While trying to unify type (" <> show t1 <> ") with (" <> show t2 <> "):\n"
      <> formatErr err
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
        , Lam "v" 0
        , App (Lam "v" $ V "v") 0
        , App (If (B True) (Lam "v" $ V "v") (Lam "v2" 42)) 0
        , App (If (B True) (Lam "v" $ V "v") (Lam "v2" $ V "v2")) 0
        , App (If (B True) (Lam "v" $ V "v") (Lam "v2" $ V "v2")) (B True)
        , Lam "x" $ App (If (B True) (Lam "v" $ V "v" + V "v") (Lam "v2" $ V "v2")) $ V "x"
        ]
      types =
        [ TBool
        , TInt
        , TArrow TInt TBool
        , TArrow TBool TInt
        , TArrow TInt TInt
        , TArrow (TVar "a") (TVar "a")
        , TArrow (TVar "a") (TVar "b")
        , TArrow (TVar "a") TInt
        ]
    in [(e, ty) | e <- exprs, ty <- types]


-- Replaces all unficiation variables with actual types.
substituteTypesInExpr :: Substitution -> TypedExpr -> TypedExpr
substituteTypesInExpr subst = \case
  I' t i -> I' (substituteType subst t) i
  B' t b -> B' (substituteType subst t) b
  V' t v -> V' (substituteType subst t) v
  If' t c th el ->
    If' (substituteType subst t)
        (substituteTypesInExpr subst c)
        (substituteTypesInExpr subst th)
        (substituteTypesInExpr subst el)
  Lam' t v e ->
    Lam' (substituteType subst t) v (substituteTypesInExpr subst e)
  App' t f arg ->
    App' (substituteType subst t)
         (substituteTypesInExpr subst f)
         (substituteTypesInExpr subst arg)
  TyAnn' e t ->
    TyAnn' (substituteTypesInExpr subst e)
           (substituteType subst t)

main :: IO ()
main = for_ scenarios $ \(e, t) -> do
  putStrLn $ "Scenario: checking expr (" <> (show e :: Text)
          <> ") to have type (" <> (show t :: Text) <> "):"
  case runTC typeEnv $ check t e of
    (_, Left err) -> putStrLn $ formatErr err
    (subst, Right typedExpr) ->
      let typedExpr' = substituteTypesInExpr subst typedExpr
      in putStrLn ("Success: " <> show typedExpr' :: Text)
