{-# LANGUAGE LambdaCase #-}

module Interpreter.IUtil
  ( lookupDefinition,
    lookupDefinitionName,
  )
where

import Data.Foldable
import SemanticAnalyzer

lookupDefinition :: SemProgram -> Identifier -> Maybe SemExpression
lookupDefinition (SemProgram _ parts) ident = definition >>= getExpression
  where
    definition :: Maybe SemProgramPart
    definition =
      find
        ( \case
            SemDeclaration -> False
            SemDefinition ident' _ -> ident' == ident
        )
        parts
    getExpression :: SemProgramPart -> Maybe SemExpression
    getExpression (SemDefinition _ expr) = Just expr
    getExpression _ = Nothing

lookupDefinitionName :: SemProgram -> String -> Maybe SemExpression
lookupDefinitionName (SemProgram _ parts) name = definition >>= getExpression
  where
    definition :: Maybe SemProgramPart
    definition =
      find
        ( \case
            SemDeclaration -> False
            SemDefinition (Identifier _ name') _ -> name' == name
        )
        parts
    getExpression :: SemProgramPart -> Maybe SemExpression
    getExpression (SemDefinition _ expr) = Just expr
    getExpression _ = Nothing
