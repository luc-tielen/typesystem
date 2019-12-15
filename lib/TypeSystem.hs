
module TypeSystem ( check ) where

import Protolude hiding ( Type, TypeError, check )
import qualified Data.Map as Map
import TypeSystem.Monad
import TypeSystem.Types
import TypeSystem.Skolems
import TypeSystem.Unification


check :: Type -> Expr -> TypeCheckM TypedExpr
check t e = addContext (WhileChecking t e) (check' t e)

check' :: Type -> Expr -> TypeCheckM TypedExpr
check' (TForAll name _ ty) expr = do
  scope <- newSkolemScope
  skolem <- newSkolemConstant
  let ty' = replaceVarWithSkolem name skolem scope ty
      expr' = replaceVarWithSkolemInExpr name skolem scope expr
  typedExpr <- check ty' expr'
  pure $ overrideType (TForAll name scope ty) typedExpr
check' unknown@(TUnknown _) e = do
  e' <- infer e
  let ty = typeOf e'
  ty' <- instantiatePolyTypeWithUnknowns ty
  unifyType ty' unknown
  pure $ overrideType ty' e'
check' expectedType e1@(Lam v e2) =
  case expectedType of
    t@(TArrow t1 t2) -> local (Map.insert v t1) $ do
      e2' <- check t2 e2
      pure $ Lam' t v e2'
    _ -> throwError $ ExpectedArrowType e1 expectedType
check' expectedType (If c t f) = do
  c' <- check TBool c
  t' <- check expectedType t
  f' <- check expectedType f
  pure $ If' expectedType c' t' f'
check' expectedType (V var) = do
  varType <- introduceSkolemScope <=< lookupVariable $ var
  expectedType' <- introduceSkolemScope expectedType
  unifyType expectedType' varType
  pure $ V' expectedType' var
check' expectedType (App f arg) = do
  f' <- infer f
  let fnType = typeOf f'
  app <- checkFunctionApplication f' fnType arg
  unifyType expectedType (typeOf app)
  pure app
check' expectedType (TyAnn e ty) = do
  expectedType' <- introduceSkolemScope expectedType
  ty' <- introduceSkolemScope ty
  unifyType expectedType' ty'
  check ty' e
check' _ Hole = do
  env <- ask
  throwError $ FoundHole env
check' expectedType e = do
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
  V var -> do
    ty <- introduceSkolemScope <=< lookupVariable $ var
    pure $ V' ty var
  Lam v body -> do
    ty <- freshUnificationVar
    local (Map.insert v ty) $ do
      body' <- infer body
      let bodyType = typeOf body'
      bodyType' <- instantiatePolyTypeWithUnknowns bodyType
      let lamType = TArrow ty bodyType'
      pure $ Lam' lamType v body'
  App f arg -> do
    f' <- infer f
    let fType = typeOf f'
    checkFunctionApplication f' fType arg
  TyAnn e ty -> do
    ty' <- introduceSkolemScope ty
    check ty' e
  If c t e -> do
    c' <- check TBool c
    t' <- infer t
    e' <- infer e
    let tyT = typeOf t'
        tyE = typeOf e'
    tyT' <- instantiatePolyTypeWithUnknowns tyT
    tyE' <- instantiatePolyTypeWithUnknowns tyE
    unifyType tyT' tyE'
    pure $ If' tyT' c' t' e'
  Hole -> do
    env <- ask
    throwError $ FoundHole env

-- | Remove any foralls in a type by introducing new unification variables.
--
-- This is necessary during type checking to avoid unifying a polymorphic type with a
-- unification variable.
instantiatePolyTypeWithUnknowns
  :: Type -> TypeCheckM Type
instantiatePolyTypeWithUnknowns (TForAll name _ ty) = do
  ty' <- replaceVarWithUnknown name ty
  instantiatePolyTypeWithUnknowns ty'
instantiatePolyTypeWithUnknowns ty = pure ty

-- | Replaces all occurrences of a var with a fresh unification variable.
replaceVarWithUnknown :: Var -> Type -> TypeCheckM Type
replaceVarWithUnknown var ty = do
  unknown <- freshUnificationVar
  pure $ replaceVarWithType var unknown ty

-- | Check the type of a function application
checkFunctionApplication :: TypedExpr -> Type -> Expr -> TypeCheckM TypedExpr
checkFunctionApplication fn fnTy arg = do
  subst <- gets substitution
  checkFunctionApplication' fn (substituteType subst fnTy) arg

checkFunctionApplication' :: TypedExpr -> Type -> Expr -> TypeCheckM TypedExpr
checkFunctionApplication' fn (TArrow argTy retTy) arg = do
  arg' <- check argTy arg
  pure $ App' retTy fn arg'
checkFunctionApplication' fn (TForAll var _ ty) arg = do
  ty' <- replaceVarWithUnknown var ty
  checkFunctionApplication fn ty' arg
checkFunctionApplication' fn ty arg = do
  argument <- do
    arg' <- infer arg
    let ty' = typeOf arg'
    ty'' <- instantiatePolyTypeWithUnknowns ty'
    pure $ overrideType ty'' arg'
  retTy <- freshUnificationVar
  let argType = typeOf argument
  unifyType ty (TArrow argType retTy)
  pure $ App' retTy fn argument

typeOf :: TypedExpr -> Type
typeOf = \case
  I' ty _ -> ty
  B' ty _ -> ty
  V' ty _ -> ty
  If' ty _ _ _ -> ty
  Lam' ty _ _ -> ty
  App' ty _ _ -> ty
  TyAnn' _ ty -> ty

overrideType :: Type -> TypedExpr -> TypedExpr
overrideType ty = \case
  I' _ i -> I' ty i
  B' _ b -> B' ty b
  V' _ v -> V' ty v
  If' _ c t f -> If' ty c t f
  Lam' _ v body -> Lam' ty v body
  App' _ f arg -> App' ty f arg
  TyAnn' e _ -> TyAnn' e ty

