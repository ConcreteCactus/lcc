{-# OPTIONS_GHC -Wincomplete-patterns #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use execState" #-}

module Interpreter (evaluateSafe, interpreterTests) where

import Errors
import Interpreter.Beta
import Interpreter.IUtil
import SemanticAnalyzer
import SyntacticAnalyzer (Literal (..), parseProgramSingleError)
import Util

evaluateSafe ::
  SemProgram ->
  Integer ->
  SemExpression ->
  Either RuntimeError SemExpression
evaluateSafe _ 0 _ = Left InfiniteLoopError
evaluateSafe prog@(SemProgram _ parts) n (SId ident@(Identifier _ identName)) =
  maybeToEither
    (lookupDefinition parts ident)
    (RUndefinedVariable identName)
    >>= evaluateSafe prog (n - 1)
evaluateSafe _ _ (SLit literal) = Right $ SLit literal
evaluateSafe _ _ (SLambda param expr) = Right $ SLambda param expr
evaluateSafe prog n (SApplication expr1 expr2) = do
  let expr1E = evaluateSafe prog (n - 1) expr1
  case expr1E of
    Left e -> Left e
    Right (SLit _) -> Left $ TypeError "applying a value to a literal"
    Right (SLambda param expr) -> do
      evaluateSafe prog (n - 1) $ beta (SApplication (SLambda param expr) expr2)
    Right a -> Right a

-- Unit tests

test :: String -> Either (Either CompilerError RuntimeError) SemExpression
test s = do
  syntactic <- sinkL $ parseProgramSingleError s
  semantic@(SemProgram _ parts) <- sinkL $ createSemanticProgram syntactic
  sinkR $
    maybeToEither
      (lookupDefinitionName parts "main")
      (RUndefinedVariable "main")
      >>= evaluateSafe semantic 1000

interpreterTests :: [Bool]
interpreterTests =
  [ test program1 == Right (SLit (IntegerLiteral 1)),
    test program2 == Right (SLit (IntegerLiteral 1))
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
