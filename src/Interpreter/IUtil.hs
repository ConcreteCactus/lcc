{-# LANGUAGE LambdaCase #-}

module Interpreter.IUtil
  ( lookupDefinition,
    lookupDefinitionName,
  )
where

import Data.Foldable
import SemanticAnalyzer

lookupDefinition :: [SemProgramPart] -> Identifier -> Maybe SemExpression
lookupDefinition parts ident = definition >>= getExpression
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

lookupDefinitionName :: [SemProgramPart] -> String -> Maybe SemExpression
lookupDefinitionName parts name = definition >>= getExpression
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
