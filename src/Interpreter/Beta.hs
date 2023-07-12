{-# OPTIONS_GHC -Wincomplete-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Interpreter.Beta (beta) where

import SemanticAnalyzer
import Util

newtype BEnv = BEnv Integer

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

beta :: SemExpression -> State BEnv SemExpression
beta (SApplication (SLambda param expr1) expr2) = do
  dupedExpr <- dupExpr expr2
  return $ replaceExpression expr1 param dupedExpr
beta a = return a

dupExpr :: SemExpression -> State BEnv SemExpression
dupExpr expression = do
  (BEnv startingCount) <- get
  put $ BEnv (startingCount + maxInd - minInd + 1)
  return $ betaHelper startingCount expression
  where
    minInd, maxInd :: Integer
    minInd = getMinIndex expression
    maxInd = getMaxIndex expression
    betaHelper :: Integer -> SemExpression -> SemExpression
    betaHelper startingCount (SId (Identifier i name)) = SId (Identifier (i - minInd + startingCount) name)
    betaHelper _ (SLit lit) = SLit lit
    betaHelper startingCount (SLambda (Identifier parami paramName) expr) =
      SLambda (Identifier (parami - minInd + startingCount) paramName) (betaHelper startingCount expr)
    betaHelper startingCount (SApplication expr1 expr2) =
      SApplication (betaHelper startingCount expr1) (betaHelper startingCount expr2)

getMinIndex :: SemExpression -> Integer
getMinIndex (SId (Identifier i _)) = i
getMinIndex (SLit _) = 0
getMinIndex (SLambda (Identifier parami _) expr) = min parami $ getMinIndex expr
getMinIndex (SApplication expr1 expr2) = min (getMinIndex expr1) $ getMinIndex expr2

getMaxIndex :: SemExpression -> Integer
getMaxIndex (SId (Identifier i _)) = i
getMaxIndex (SLit _) = 0
getMaxIndex (SLambda (Identifier parami _) expr) = max parami $ getMaxIndex expr
getMaxIndex (SApplication expr1 expr2) = max (getMaxIndex expr1) $ getMaxIndex expr2
