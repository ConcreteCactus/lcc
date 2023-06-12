{-# OPTIONS_GHC -Wincomplete-patterns #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

module Interpreter
  ( beta,
  )
where

import SemanticAnalyzer

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

beta :: SemExpression -> SemExpression
beta (SApplication (SLambda param expr1) expr2) = replaceExpression expr1 param expr2
beta a = a
