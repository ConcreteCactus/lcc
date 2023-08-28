{-# HLINT ignore "Use tuple-section" #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Interpreter (interpreterTests, evaluate) where

import Data.Foldable
import Errors
import SemanticAnalyzer
import Util
import Util.Limited

incOuterRefs :: Int -> Int -> SemExpression -> SemExpression
incOuterRefs i incby (SId j) =
  if i <= j
    then SId (j + incby)
    else SId j
incOuterRefs _ _ (SRef s) = SRef s
incOuterRefs _ _ (SLit l) = SLit l
incOuterRefs i incby (SLambda s a) = SLambda s (incOuterRefs (i + 1) incby a)
incOuterRefs i incby (SApplication a1 a2) = SApplication (incOuterRefs i incby a1) (incOuterRefs i incby a2)

substitute :: Int -> SemExpression -> SemExpression -> SemExpression
substitute i sid@(SId j) b =
  if i == j then incOuterRefs 1 (j - 1) b else sid
substitute _ (SRef s) _ = SRef s
substitute _ (SLit l) _ = SLit l
substitute i (SLambda s a) b = SLambda s (substitute (i + 1) a b)
substitute i (SApplication a1 a2) b = SApplication (substitute i a1 b) (substitute i a2 b)

beta :: SemExpression -> SemExpression -> SemExpression
beta (SLambda _ a) b = substitute 1 a b
beta _ _ = error "beta: not a lambda"

alpha :: SemExpression -> SemExpression -> Bool
alpha = (==)

evaluate :: [Definition] -> SemExpression -> Limited (RunErrorable SemExpression)
evaluate parts (SApplication expr1 expr2) = do
  decLimit
  expr1Eval <- evaluate parts expr1
  case expr1Eval of
    Left e -> return $ Left e
    Right fn@(SLambda _ _) -> evaluate parts (beta fn expr2)
    Right (SRef refName) -> case lookupRefExp parts refName of
      Nothing -> return $ Left $ RUndefinedReference refName
      Just expr -> evaluate parts (SApplication expr expr2)
    Right a -> return $ Left $ RTypeError (show a ++ " is not a function")
evaluate _ a = return $ pure a

runLimitedComputation :: Limit -> Limited (RunErrorable a) -> RunAndCompileErrorable a
runLimitedComputation lim comp = case runLimited lim comp of
  Just a -> sinkR a
  Nothing -> Left $ Right ComputationLimitReached

-- Unit tests

testEvalWithProgram :: String -> String -> RunAndCompileErrorable SemExpression
testEvalWithProgram progS exprS = do
  prog@(SemProgram _ parts) <- sinkL $ parseAndCreateProgram progS
  expr <- sinkL $ parseAndCreateExpressionWithProgram prog exprS
  runLimitedComputation (Limit 1000) (evaluate parts expr)

interpreterTests :: [Bool]
interpreterTests = betaTests ++ evaluateTests

betaTests :: [Bool]
betaTests =
  [ beta (SLambda "" (SId 1)) (SId 4) == SId 4,
    beta (SLambda "" (SLambda "" (SId 1))) (SId 4) == SLambda "" (SId 1),
    beta (SLambda "" (SLambda "" (SId 2))) (SId 4) == SLambda "" (SId 5),
    beta (SLambda "" (SLambda "" (SLambda "" (SId 3)))) (SLambda "" (SId 2)) == SLambda "" (SLambda "" (SLambda "" (SId 4))),
    beta (SLambda "" (SLambda "" (SLambda "" (SId 3)))) (SId 2) == SLambda "" (SLambda "" (SId 4)),
    incOuterRefs 1 0 (SId 1) == SId 1,
    incOuterRefs 1 1 (SId 2) == SId 3,
    incOuterRefs 1 2 (SLambda "" (SId 1)) == SLambda "" (SId 1),
    incOuterRefs 1 2 (SLambda "" (SId 2)) == SLambda "" (SId 4)
  ]

evaluateTests :: [Bool]
evaluateTests =
  [ testEvalWithProgram program1 "one" == Right (SRef "one"),
    testEvalWithProgram program1 "suc zero" == Right (SLambda "f" (SLambda "z" (SApplication (SId 2) (SApplication (SApplication (SRef "zero") (SId 2)) (SId 1))))),
    testEvalWithProgram program1 "y y" == Left (Right ComputationLimitReached)
  ]

program1 :: String
program1 =
  "zero := \\f.\\z.z\n"
    ++ "suc := \\n.\\f.\\z.f (n f z)\n"
    ++ "one := suc zero\n"
    ++ "y := \\f. (\\x. f (x x)) (\\x. f (x x))\n"
