{-# LANGUAGE LambdaCase #-}

module Interpreter.IUtil
  ( lookupDefinition,
    lookupDefinitionName,
  )
where

import Data.Foldable
import SemanticAnalyzer

lookupDefinition :: [SemProgramPart] -> Identifier -> Maybe SemExpression
lookupDefinition prog ident = definition >>= getExpression
  where
    definition :: Maybe SemProgramPart
    definition =
      find
        ( \case
            SemDeclaration -> False
            SemDefinition ident' _ -> ident' == ident
        )
        prog
    getExpression :: SemProgramPart -> Maybe SemExpression
    getExpression (SemDefinition _ expr) = Just expr
    getExpression _ = Nothing

lookupDefinitionName :: [SemProgramPart] -> String -> Maybe SemExpression
lookupDefinitionName prog name = definition >>= getExpression
  where
    definition :: Maybe SemProgramPart
    definition =
      find
        ( \case
            SemDeclaration -> False
            SemDefinition (Identifier _ name') _ -> name' == name
        )
        prog
    getExpression :: SemProgramPart -> Maybe SemExpression
    getExpression (SemDefinition _ expr) = Just expr
    getExpression _ = Nothing
