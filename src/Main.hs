
{-# OPTIONS_GHC -Wno-orphans #-}

module Main ( main ) where

import Protolude hiding ( Type, TypeError, check )
import TypeSystem.Monad
import TypeSystem.Types
import TypeSystem.Errors
import TypeSystem.Unification
import TypeSystem
import qualified Data.Map as Map


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
