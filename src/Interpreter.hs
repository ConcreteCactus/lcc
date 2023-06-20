{-# OPTIONS_GHC -Wincomplete-patterns #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

module Interpreter
  ( beta,
  )
where

import SemanticAnalyzer
import SyntacticAnalyzer (Literal)

data EvaluatedExpression
  = ELit Literal
  | EId Identifier
  | ELambda Identifier SemExpression

data RuntimeError = InfiniteLoopError | TypeError

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

makeEvaluated :: SemExpression -> Maybe EvaluatedExpression
makeEvaluated (SLit literal) = Just $ ELit literal
makeEvaluated (SId identifier) = Just $ EId identifier
makeEvaluated (SLambda identifier semExpr) = Just $ ELambda identifier semExpr
makeEvaluated (SApplication _ _) = Nothing

evaluateSafe :: Integer -> SemExpression -> Either RuntimeError EvaluatedExpression
evaluateSafe n _ | n < 0 = Left InfiniteLoopError
evaluateSafe 0 app@(SApplication (SLambda ident semExpr1) semExpr2) = Left InfiniteLoopError
evaluateSafe
  n
  app@(SApplication (SLambda ident semExpr1) semExpr2) =
    evaluateSafe (n - 1) $ beta app
evaluateSafe n (SApplication _ _) = Left TypeError
evaluateSafe n a = maybeToEither (makeEvaluated a) InfiniteLoopError

maybeToEither :: Maybe a -> e -> Either e a
maybeToEither Nothing e = Left e
maybeToEither (Just a) _ = Right a
