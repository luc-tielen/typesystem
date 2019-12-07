
module TypeSystem.Unification
  ( freshUnificationVar
  , unifyType
  , substituteType
  ) where

import Protolude hiding ( Type )
import qualified Data.IntMap as IntMap
import qualified Data.List as List
import TypeSystem.Types
import TypeSystem.Monad

-- Generate a fresh unification variable
freshUnificationVar :: TypeCheckM Type
freshUnificationVar = do
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
