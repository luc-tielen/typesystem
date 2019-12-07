
module TypeSystem.Monad
  ( Substitution
  , CheckState(..)
  , TypeCheckM
  , runTC
  , addContext
  ) where

import Protolude hiding ( Type, TypeError )
import Control.Monad.RWS
import TypeSystem.Types


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

