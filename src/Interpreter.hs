{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wincomplete-patterns #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

module Interpreter
  ( EvaluatedExpression (..),
    evaluateSafe,
  )
where

import Data.List (find)
import Errors
import SemanticAnalyzer
import SyntacticAnalyzer (Literal (..), parseProgramSingleError)
import Util

data EvaluatedExpression
  = ELit Literal
  | ELambda Identifier SemExpression
  deriving (Eq, Show)

replaceExpression :: SemExpression -> Identifier -> SemExpression -> SemExpression
replaceExpression (SId eid) curid repexpr =
  if curid
    == eid
    then repexpr
    else SId eid
replaceExpression (SLambda param expr) curid repexpr =
  SLambda param $
    replaceExpression expr curid repexpr
replaceExpression (SApplication expr1 expr2) curid repid =
  SApplication
    (replaceExpression expr1 curid repid)
    (replaceExpression expr2 curid repid)
replaceExpression (SLit l) _ _ = SLit l

beta :: SemExpression -> SemExpression
beta (SApplication (SLambda param expr1) expr2) = replaceExpression expr1 param expr2
beta a = a

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

evaluateSafe :: SemProgram -> Integer -> SemExpression -> Either RuntimeError EvaluatedExpression
evaluateSafe prog n (SId (Identifier _ name)) =
  maybeToEither
    (lookupDefinitionName prog name)
    UndefinedVariableError
    >>= evaluateSafe prog (n - 1)
evaluateSafe _ _ (SLit literal) = Right $ ELit literal
evaluateSafe _ _ (SLambda param expr) = Right $ ELambda param expr
evaluateSafe prog n (SApplication expr1 expr2) =
  case evaluateSafe prog (n - 1) expr1 of
    Left e -> Left e
    Right (ELit _) -> Left TypeError
    Right (ELambda param expr) ->
      evaluateSafe prog (n - 1) $
        beta (SApplication (SLambda param expr) expr2)

maybeToEither :: Maybe a -> e -> Either e a
maybeToEither Nothing e = Left e
maybeToEither (Just a) _ = Right a

-- Unit tests

test :: String -> Either (Either CompilerError RuntimeError) EvaluatedExpression
test s = do
  syntactic <- sinkL $ parseProgramSingleError s
  semantic <- sinkL $ createSemanticProgram syntactic
  sinkR $
    maybeToEither
      (lookupDefinitionName semantic "main")
      UndefinedVariableError
      >>= evaluateSafe semantic 1000

tests :: [Bool]
tests =
  [ test program1 == Right (ELit (IntegerLiteral 1)),
    test program2 == Right (ELit (IntegerLiteral 1))
  ]

program1 :: String
program1 =
  "true := \\a.\\b.a\n"
    ++ "false := \\a.\\b.b\n"
    ++ "main := true (false 0 1) 2"

program2 :: String
program2 =
  "true := \\a.\\b.a\n"
    ++ "false := \\a.\\b.b\n"
    ++ "and := \\a.\\b. a b false\n"
    ++ "or := \\a.\\b. a true b\n"
    ++ "not := \\a. a false true\n"
    ++ "xor := \\a.\\b. a (not b) b\n"
    ++ "main := xor true false (or false false 0 1) 0"
