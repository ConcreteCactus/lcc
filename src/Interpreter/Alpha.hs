module Interpreter.Alpha (alpha) where

import Errors
import Interpreter.Beta
import Interpreter.IUtil
import SemanticAnalyzer
import SyntacticAnalyzer
import Util

type BoundVars = [Identifier]

data AlphaExpression
  = ALit Literal
  | AId Integer
  | ALambda Integer AlphaExpression
  | AApplication AlphaExpression AlphaExpression
  deriving (Eq)

instance Show AlphaExpression where
  show (ALit literal) = show literal
  show (AId ident) = show ident
  show (ALambda param expr) = "λ" ++ show param ++ "." ++ show expr
  show (AApplication expr1@(ALambda _ _) expr2@(AApplication _ _)) =
    "(" ++ show expr1 ++ ") (" ++ show expr2 ++ ")"
  show (AApplication expr1@(ALambda _ _) expr2) =
    "(" ++ show expr1 ++ ") " ++ show expr2
  show (AApplication expr1 expr2@(AApplication _ _)) =
    show expr1 ++ " (" ++ show expr2 ++ ")"
  show (AApplication expr1 expr2) = show expr1 ++ " " ++ show expr2

evaluateSafeDeep :: Integer -> SemExpression -> Either RuntimeError SemExpression
evaluateSafeDeep 0 _ = Left InfiniteLoopError
evaluateSafeDeep _ (SId ident) = Right $ SId ident
evaluateSafeDeep _ (SLit lit) = Right $ SLit lit
evaluateSafeDeep n (SLambda param expr) = SLambda param <$> evaluateSafeDeep (n - 1) expr
evaluateSafeDeep n (SApplication expr1 expr2) =
  let expr1e = evaluateSafeDeep (n - 1) expr1
   in case expr1e of
        Left e -> Left e
        Right lamb@(SLambda _ _) ->
          evaluateSafeDeep (n - 1) (beta $ SApplication lamb expr2)
        Right a -> SApplication a <$> evaluateSafeDeep (n - 1) expr2

replaceReferences ::
  SemProgram ->
  BoundVars ->
  Integer ->
  SemExpression ->
  Either RuntimeError SemExpression
replaceReferences _ _ 0 _ = Left InfiniteLoopError
replaceReferences prog bvars n (SId ident@(Identifier _ name)) =
  if ident `elem` bvars
    then Right $ SId ident
    else maybeToEither (lookupDefinitionName prog name) UndefinedVariableError >>= replaceReferences prog bvars (n - 1)
replaceReferences _ _ _ (SLit lit) = Right $ SLit lit
replaceReferences prog bvars n (SLambda param expr) =
  SLambda param <$> replaceReferences prog (param : bvars) (n - 1) expr
replaceReferences prog bvars n (SApplication expr1 expr2) =
  SApplication
    <$> replaceReferences prog bvars (n - 1) expr1
    <*> replaceReferences prog bvars (n - 1) expr2

normalizeToAlphaS :: SemExpression -> State ([(Identifier, Integer)], Integer) AlphaExpression
normalizeToAlphaS (SId identifier) = do
  (idents, count) <- get
  case lookup identifier idents of
    Nothing -> do
      put ((identifier, count + 1) : idents, count + 1)
      return $ AId $ count + 1
    Just integer -> do
      return $ AId integer
normalizeToAlphaS (SLit lit) = return $ ALit lit
normalizeToAlphaS (SLambda param expr) = do
  (idents, count) <- get
  put ((param, count + 1) : idents, count + 1)
  next <- normalizeToAlphaS expr
  return $ ALambda (count + 1) next
normalizeToAlphaS (SApplication expr1 expr2) =
  AApplication
    <$> normalizeToAlphaS expr1
    <*> normalizeToAlphaS expr2

normalizeToAlpha :: SemExpression -> AlphaExpression
normalizeToAlpha rexp = execState (normalizeToAlphaS rexp) ([], 0)

normalizePartial :: SemProgram -> SemExpression -> Either RuntimeError SemExpression
normalizePartial prog expr = replaceReferences prog [] 1000 expr >>= evaluateSafeDeep 1000

alpha :: SemProgram -> SemExpression -> SemExpression -> Bool
alpha prog expr1 expr2 = case (alpha1, alpha2) of
  (Left _, _) -> False
  (_, Left _) -> False
  (Right a, Right b) -> a == b
  where
    alpha1 = normalizeToAlpha <$> normalizePartial prog expr1
    alpha2 = normalizeToAlpha <$> normalizePartial prog expr2
