
module TypeSystem.Monad
  ( Substitution
  , CheckState(..)
  , TypeCheckM
  , runTC
  , addContext
  , lookupVariable
  , replaceVarWithType
  ) where

import Protolude hiding ( Type, TypeError )
import Control.Monad.RWS
import TypeSystem.Types
import qualified Data.Set as Set
import qualified Data.Map as Map


type Substitution = IntMap Type

data CheckState =
  CheckState
  { nextUnificationVar :: Int
  , nextSkolemVar :: Int
  , nextSkolemScope :: Int
  , substitution :: Substitution
  } deriving Show

type TypeCheckM = ExceptT TypeError (RWS TypeEnv () CheckState)


runTC :: TypeEnv -> TypeCheckM a -> (Substitution, Either TypeError a)
runTC env m =
  let (res, cs, _) = runRWS (runExceptT m) env (CheckState 0 0 0 mempty)
      subst = substitution cs
   in (subst, res)

addContext :: (TypeError -> TypeError) -> TypeCheckM a -> TypeCheckM a
addContext ctx m = catchError m (throwError . ctx)

-- | Looks up a variable in the type environment.
-- Throws an error if the environment does not contain a variable with the given name.
lookupVariable :: Var -> TypeCheckM Type
lookupVariable var = asks (Map.lookup var) >>= \case
  Nothing -> throwError $ UnboundVariable var
  Just ty -> pure ty

replaceVarWithType :: Var -> Type -> (Type -> Type)
replaceVarWithType var replacement t = runReader (go t) Set.empty where
  go = \case
    TVar var' | var == var' -> do
      skolemsInScope <- ask
      pure $ if var `elem` skolemsInScope then TVar var else replacement
    TForAll var' scope' ty -> local (Set.insert var') $ do
      ty' <- go ty
      pure $ TForAll var' scope' ty'
    TArrow t1 t2 -> TArrow <$> go t1 <*> go t2
    ty' -> pure ty'

