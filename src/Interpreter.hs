{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wincomplete-patterns #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use execState" #-}

module Interpreter (evaluateSafe) where

import Data.List (find)
import Errors
import Interpreter.Beta
import Interpreter.IUtil
import SemanticAnalyzer
import SyntacticAnalyzer (Literal (..), parseProgramSingleError)
import Util

data IEnv = IEnv Integer

getIncIEnv :: State IEnv Integer
getIncIEnv = do
  (IEnv i) <- get
  put $ IEnv (i + 1)
  return i

evaluateSafe :: SemProgram -> Integer -> SemExpression -> Either RuntimeError SemExpression
evaluateSafe _ 0 _ = Left InfiniteLoopError
evaluateSafe prog n (SId (Identifier _ name)) =
  maybeToEither
    (lookupDefinitionName prog name)
    UndefinedVariableError
    >>= evaluateSafe prog (n - 1)
evaluateSafe _ _ (SLit literal) = Right $ SLit literal
evaluateSafe _ _ (SLambda param expr) = Right $ SLambda param expr
evaluateSafe prog n (SApplication expr1 expr2) =
  case evaluateSafe prog (n - 1) expr1 of
    Left e -> Left e
    Right (SLit _) -> Left $ TypeError "applying a value to a literal"
    Right (SLambda param expr) ->
      evaluateSafe prog (n - 1) $
        beta (SApplication (SLambda param expr) expr2)
    Right a -> Right a

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

tests :: [Bool]
tests =
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
