module Interpreter.Alpha (alpha, normalizePartial, alphaTests) where

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

evaluateSafeDeepS ::
  Integer ->
  SemExpression ->
  State BEnv (Either RuntimeError SemExpression)
evaluateSafeDeepS 0 _ = return $ Left InfiniteLoopError
evaluateSafeDeepS _ (SId ident) = return $ Right $ SId ident
evaluateSafeDeepS _ (SLit lit) = return $ Right $ SLit lit
evaluateSafeDeepS n (SLambda param expr) = do
  evaluated <- evaluateSafeDeepS (n - 1) expr
  return $ SLambda param <$> evaluated
evaluateSafeDeepS n (SApplication expr1 expr2) = do
  expr1e <- evaluateSafeDeepS (n - 1) expr1
  case expr1e of
    Left e -> return $ Left e
    Right lamb@(SLambda _ _) -> do
      exprBeta <- beta (SApplication lamb expr2)
      evaluateSafeDeepS (n - 1) exprBeta
    Right a -> do
      evaluated <- evaluateSafeDeepS (n - 1) expr2
      return $ SApplication a <$> evaluated

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
normalizePartial (SemProgram env@(Env _ count) parts) expr = do
  replaced <- replaceReferences (SemProgram env parts) [] 1000 expr
  execState (evaluateSafeDeepS 1000 replaced) (BEnv count)

alpha :: SemProgram -> SemExpression -> SemExpression -> Bool
alpha prog expr1 expr2 = case (alpha1, alpha2) of
  (Left _, _) -> False
  (_, Left _) -> False
  (Right a, Right b) -> a == b
  where
    alpha1 = normalizeToAlpha <$> normalizePartial prog expr1
    alpha2 = normalizeToAlpha <$> normalizePartial prog expr2

-- Unit tests

test :: String -> String -> String -> Either CompilerError Bool
test s e1 e2 = do
  prog <- parseAndCreateProgram s
  expr1 <- parseAndCreateExpressionWithProgram prog e1
  expr2 <- parseAndCreateExpressionWithProgram prog e2
  return $ alpha prog expr1 expr2

alphaTests :: [Bool]
alphaTests = [test program1 "one" "suc zero" == Right True]

program1 :: String
program1 =
  "zero := \\f.\\z.z\n"
    ++ "suc := \\n.\\f.\\z.f (n f z)\n"
    ++ "one := \\f.\\z.f z\n"
