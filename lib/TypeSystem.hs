
module TypeSystem ( check ) where

import Protolude hiding ( Type, TypeError, check )
import qualified Data.Map as Map
import TypeSystem.Monad
import TypeSystem.Types
import TypeSystem.Unification


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
    ty <- freshUnificationVar
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

typeOf :: TypedExpr -> Type
typeOf = \case
  I' ty _ -> ty
  B' ty _ -> ty
  V' ty _ -> ty
  If' ty _ _ _ -> ty
  Lam' ty _ _ -> ty
  App' ty _ _ -> ty
  TyAnn' _ ty -> ty

