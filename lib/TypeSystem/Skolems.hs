
module TypeSystem.Skolems
  ( newSkolemConstant
  , newSkolemScope
  , introduceSkolemScope
  , replaceVarWithSkolem
  , replaceVarWithSkolemInExpr
  , checkForEscapedSkolems
  ) where

import Protolude hiding ( Type, TypeError )
import TypeSystem.Monad
import TypeSystem.Types
import qualified Data.Set as Set


-- | Generate a new skolem constant
newSkolemConstant :: TypeCheckM Skolem
newSkolemConstant = do
  skolem <- gets nextSkolemVar
  modify $ \st -> st { nextSkolemVar = skolem + 1 }
  pure $ Skolem skolem

-- | Introduce skolem scope at every occurence of a ForAll
introduceSkolemScope :: Type -> TypeCheckM Type
introduceSkolemScope = \case
  TForAll name Nothing ty -> do
    scope <- Just <$> newSkolemScope
    pure $ TForAll name scope ty
  TArrow t1 t2 ->
    TArrow <$> introduceSkolemScope t1
           <*> introduceSkolemScope t2
  other -> pure other

-- | Generate a new skolem scope
newSkolemScope :: TypeCheckM SkolemScope
newSkolemScope = do
  scope <- gets nextSkolemScope
  modify $ \st -> st { nextSkolemScope = scope + 1 }
  pure $ SkolemScope scope

-- | Skolemize a type variable by replacing all variables of the same name
--   in a type with a skolem (rigid type variable).
--
--   This function takes name shadowing introduced by foralls into account.
--   If no shadowing can occur (due to previous semantic analysis step),
--   this function could be significantly simplified by just replacing all vars with a name.
replaceVarWithSkolem :: Text -> Skolem -> SkolemScope -> Type -> Type
replaceVarWithSkolem var skolem scope =
  replaceVarWithType var (TSkolem var scope skolem)

-- | This function skolemizes type variables with the same name appearing
--   in any type signatures in a given expression.
replaceVarWithSkolemInExpr :: Text -> Skolem -> SkolemScope -> Expr -> Expr
replaceVarWithSkolemInExpr var skolem skolemScope = go where
  go = \case
    If c t f -> If (go c) (go t) (go f)
    Lam v body -> Lam v (go body)
    App f arg -> App (go f) (go arg)
    TyAnn e ty ->
      let ty' = replaceVarWithSkolem var skolem skolemScope ty
       in TyAnn (go e) ty'
    e -> e

-- | Checks if any skolem variables escaped their scope and throws an error
--   if this is the case.
--
-- Every skolem variable is created when a 'ForAll' type is skolemized.
-- This determines the scope of that skolem variable, which is copied from
-- the 'SkolemScope' field of the 'ForAll' constructor.
--
-- This function traverses the tree top-down, and collects any 'SkolemScope's
-- introduced by 'ForAll's. If a 'Skolem' is encountered whose 'SkolemScope' is
-- not in the current list, then we have found an escaped skolem variable.
checkForEscapedSkolems :: Expr -> TypeCheckM ()
checkForEscapedSkolems expr = runReaderT (go expr) Set.empty where
  -- TODO handling of scopes between multiple "go" calls
  -- let could introduce extra skolems that would have to be stored in Set?
  go = \case
    If c t e -> go c *> go t *> go e
    Lam _ body -> go body
    App f arg -> go f *> go arg
    TyAnn e ty -> do
      checkForEscapedSkolems' ty
      go e
    _ -> pure ()

-- | On a single type signature, checks if skolems escaped their scope.
checkForEscapedSkolems' :: Type -> ReaderT (Set SkolemScope) TypeCheckM ()
checkForEscapedSkolems' ty = do
  scopes <- asks (collectScopes ty <>)
  let errors = [ EscapedSkolem name ty
               | (name, scope) <- collectSkolems ty
               , scope `notElem` scopes
               ]
  case nonEmpty errors of
    Nothing -> pure ()
    Just errors' -> throwError $ MultipleErrors errors'
  where
    collectScopes = \case
      TForAll _ (Just scope) t -> Set.insert scope $ collectScopes t
      TForAll {} -> panic "Found unitialized skolem scope."
      _ -> mempty
    collectSkolems = \case
      TForAll _ _ t -> collectSkolems t
      TArrow t1 t2 -> collectSkolems t1 <> collectSkolems t2
      TSkolem name scope _ -> [(name, scope)]
      _ -> mempty

