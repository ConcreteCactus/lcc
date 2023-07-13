{-# OPTIONS_GHC -Wincomplete-patterns #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use execState" #-}

module Interpreter (evaluateSafeS, interpreterTests) where

import Errors
import Interpreter.Beta
import Interpreter.IUtil
import SemanticAnalyzer
import SyntacticAnalyzer (Literal (..), parseProgramSingleError)
import Util

evaluateSafeS ::
  SemProgram ->
  Integer ->
  SemExpression ->
  State BEnv (Either RuntimeError SemExpression)
evaluateSafeS _ 0 _ = return $ Left InfiniteLoopError
evaluateSafeS prog n (SId (Identifier _ name)) = do
  let defM = lookupDefinitionName prog name
  case defM of
    Nothing -> return $ Left UndefinedVariableError
    Just def -> evaluateSafeS prog (n - 1) def
evaluateSafeS _ _ (SLit literal) = return $ Right $ SLit literal
evaluateSafeS _ _ (SLambda param expr) = return $ Right $ SLambda param expr
evaluateSafeS prog n (SApplication expr1 expr2) = do
  expr1E <- evaluateSafeS prog (n - 1) expr1
  case expr1E of
    Left e -> return $ Left e
    Right (SLit _) -> return $ Left $ TypeError "applying a value to a literal"
    Right (SLambda param expr) -> do
      exprBeta <- beta (SApplication (SLambda param expr) expr2)
      evaluateSafeS prog (n - 1) exprBeta
    Right a -> return $ Right a

evaluateSafe ::
  SemProgram ->
  Integer ->
  SemExpression ->
  Either RuntimeError SemExpression
evaluateSafe (SemProgram env@(Env _ count) parts) n expr =
  execState
    (evaluateSafeS (SemProgram env parts) n expr)
    (BEnv count)

-- Unit tests

test :: String -> Either (Either CompilerError RuntimeError) SemExpression
test s = do
  syntactic <- sinkL $ parseProgramSingleError s
  semantic <- sinkL $ createSemanticProgram syntactic
  sinkR $
    maybeToEither
      (lookupDefinitionName semantic "main")
      UndefinedVariableError
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
