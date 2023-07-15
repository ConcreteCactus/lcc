{-# OPTIONS_GHC -Wincomplete-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Interpreter.Beta (beta) where

import SemanticAnalyzer
import SyntacticAnalyzer

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

-- Unit tests

betaTests :: [Bool]
betaTests =
  [ beta (SApplication (SLambda (Identifier 0 "x") (SId (Identifier 0 "x"))) (SLit (IntegerLiteral 0))) == SLit (IntegerLiteral 0)
  ]
