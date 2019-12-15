
module TypeSystem.Errors
  ( formatErr
  , formatTypeEnv
  ) where

import Protolude hiding ( Type, TypeError )
import TypeSystem.Types
import qualified Data.Text as T
import qualified Data.Map as Map

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
  EscapedSkolem var ty ->
    "Found a skolem (rigid type variable) '" <> show var <>
    "' that escaped it's scope in the type:\n  " <> show ty
  MultipleErrors errors ->
    "Found the following errors:\n" <>
      T.intercalate "\n\n" (map formatErr $ toList errors)
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

