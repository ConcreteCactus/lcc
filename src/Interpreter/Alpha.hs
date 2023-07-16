module Interpreter.Alpha (alpha, normalizePartial, alphaTests) where

import Data.Maybe
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

newtype DEnv = DEnv Integer

dupExpr :: [SemProgramPart] -> SemExpression -> State DEnv SemExpression
dupExpr parts expression = do
  (DEnv startingCount) <- get
  put $ DEnv (startingCount + maxInd - minInd + 1)
  return $ dupHelper startingCount expression
  where
    minInd, maxInd :: Integer
    minInd = getMinIndex expression
    maxInd = getMaxIndex expression
    dupHelper :: Integer -> SemExpression -> SemExpression
    dupHelper startingCount (SId ident@(Identifier i name)) =
      let lookupM = lookupDefinition parts ident
       in case lookupM of
            Just _ -> SId (Identifier i name)
            Nothing -> SId (Identifier (i - minInd + startingCount) name)
    dupHelper _ (SLit lit) = SLit lit
    dupHelper startingCount (SLambda (Identifier parami paramName) expr) =
      SLambda (Identifier (parami - minInd + startingCount) paramName) (dupHelper startingCount expr)
    dupHelper startingCount (SApplication expr1 expr2) =
      SApplication (dupHelper startingCount expr1) (dupHelper startingCount expr2)

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

evaluateShallowS ::
  [SemProgramPart] ->
  Integer ->
  SemExpression ->
  State DEnv (Either RuntimeError SemExpression)
evaluateShallowS _ 0 _ = return $ Left InfiniteLoopError
evaluateShallowS _ _ (SId ident) = return $ Right $ SId ident
evaluateShallowS _ _ (SLit lit) = return $ Right $ SLit lit
evaluateShallowS _ _ (SLambda param expr) = return $ Right $ SLambda param expr
evaluateShallowS parts n (SApplication expr1 expr2) = do
  expr1E <- evaluateShallowS parts (n - 1) expr1
  case expr1E of
    Left e -> return $ Left e
    Right lamb@(SLambda _ _) ->
      evaluateShallowS parts (n - 1) $ beta (SApplication lamb expr2)
    Right (SId ident)
      | isJust (lookupDefinition parts ident) ->
          let lookupM = lookupDefinition parts ident
           in case lookupM of
                Nothing -> error "Guard failed"
                Just definition -> do
                  duped <- dupExpr parts definition
                  evaluateShallowS parts (n - 1) $ SApplication duped expr2
    Right a -> return $ Right $ SApplication a expr2

evaluateDeepS ::
  [SemProgramPart] ->
  Integer ->
  SemExpression ->
  State DEnv (Either RuntimeError SemExpression)
evaluateDeepS _ 0 _ = return $ Left InfiniteLoopError
evaluateDeepS _ _ (SId ident) = return $ Right $ SId ident
evaluateDeepS _ _ (SLit lit) = return $ Right $ SLit lit
evaluateDeepS parts n (SLambda param expr1) = do
  expr2E <- evaluateShallowS parts (n - 1) expr1
  case expr2E of
    Left e -> return $ Left e
    Right expr2 -> (SLambda param <$>) <$> evaluateDeepS parts (n - 1) expr2
evaluateDeepS parts n (SApplication expr1 expr2) = do
  applShallowE <- evaluateShallowS parts (n - 1) (SApplication expr1 expr2)
  case applShallowE of
    Left e -> return $ Left e
    Right (SApplication expr3 expr4) ->
      (SApplication expr3 <$>)
        <$> evaluateDeepS parts (n - 1) expr4
    Right a -> evaluateDeepS parts (n - 1) a

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
normalizePartial (SemProgram (Env _ count) parts) expr =
  execState (evaluateDeepS parts 1000 expr) (DEnv count)

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

testDup :: String -> Integer -> Either CompilerError SemExpression
testDup expr st = do
  semExpr <- parseAndCreateExpressionWithProgram emptyProgram expr
  return $ execState (dupExpr [] semExpr) (DEnv st)

alphaTests :: [Bool]
alphaTests =
  [ getMinIndex (SId (Identifier 0 "x")) == 0,
    getMinIndex (SLit (IntegerLiteral 0)) == 0,
    getMinIndex (SLambda (Identifier 0 "x") (SId (Identifier 1 "x"))) == 0,
    getMinIndex (SApplication (SId (Identifier 0 "x")) (SId (Identifier 1 "x"))) == 0,
    getMaxIndex (SId (Identifier 0 "x")) == 0,
    getMaxIndex (SLit (IntegerLiteral 0)) == 0,
    getMaxIndex (SLambda (Identifier 0 "x") (SId (Identifier 1 "x"))) == 1,
    getMaxIndex (SApplication (SId (Identifier 0 "x")) (SId (Identifier 1 "x"))) == 1,
    test program1 "one" "suc zero" == Right True,
    testDup "\\a.\\b.a b" 0 == Right (SLambda (Identifier 0 "a") (SLambda (Identifier 1 "b") (SApplication (SId (Identifier 0 "a")) (SId (Identifier 1 "b"))))),
    testDup "\\a.\\b.a b" 10 == Right (SLambda (Identifier 10 "a") (SLambda (Identifier 11 "b") (SApplication (SId (Identifier 10 "a")) (SId (Identifier 11 "b"))))),
    test program2 "one suc zero" "one" == Right True,
    test program2 "add five five" "ten" == Right True,
    test program2 "index true onetwothree one" "one" == Right True
  ]

program1 :: String
program1 =
  "zero := \\f.\\z.z\n"
    ++ "suc := \\n.\\f.\\z.f (n f z)\n"
    ++ "one := \\f.\\z.f z\n"

program2 :: String
program2 =
  "zero := \\f.\\b.b\n"
    ++ "suc  := \\n.\\f.\\b.f (n f b)\n"
    ++ "add := \\a.\\b.\\f.\\z. a f (b f z)\n"
    ++ "pred := \\n.\\f.\\z.n (\\g.\\h.h (g f)) (\\u.z) (\\u.u)\n"
    ++ "one := suc zero\n"
    ++ "two := suc one\n"
    ++ "three := suc two\n"
    ++ "four := suc three\n"
    ++ "five := suc four\n"
    ++ "six := suc five\n"
    ++ "seven := suc six\n"
    ++ "eight := suc seven\n"
    ++ "nine := suc eight\n"
    ++ "ten := suc nine\n"
    ++ "true := \\a.\\b.a\n"
    ++ "false := \\a.\\b.b\n"
    ++ "emptyVec := \\f.\\e.e\n"
    ++ "cons := \\item.\\vec.\\f.\\e.f item vec\n"
    ++ "onetwothree := cons one (cons two (cons three emptyVec))\n"
    ++ "head := \\err.\\vec. vec (\\item.\\tail.item) err\n"
    ++ "tail := \\err.\\vec. vec (\\item.\\tail.tail) err\n"
    ++ "isZero := \\n. n (\\x. false) true\n"
    ++ "index := \\err.\\vec.\\n. (isZero n) (head err vec) (index err (tail err vec) (pred n))\n"
