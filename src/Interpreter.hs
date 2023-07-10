{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wincomplete-patterns #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use execState" #-}

module Interpreter
  ( evaluateSafe,
    alpha,
    normalizeToAlpha,
    normalizePartial,
    replaceReferences,
    evaluateSafeDeep,
  )
where

import Data.List (find)
import Errors
import SemanticAnalyzer
import SyntacticAnalyzer (Literal (..), parseProgramSingleError)
import Util

data AlphaExpression
  = ALit Literal
  | AId Integer
  | ALambda Integer AlphaExpression
  | AApplication AlphaExpression AlphaExpression
  deriving (Eq)

type BoundVars = [Identifier]

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

lookupDefinition :: [SemProgramPart] -> Identifier -> Maybe SemExpression
lookupDefinition prog ident = definition >>= getExpression
  where
    definition :: Maybe SemProgramPart
    definition =
      find
        ( \case
            SemDeclaration -> False
            SemDefinition ident' _ -> ident' == ident
        )
        prog
    getExpression :: SemProgramPart -> Maybe SemExpression
    getExpression (SemDefinition _ expr) = Just expr
    getExpression _ = Nothing

lookupDefinitionName :: [SemProgramPart] -> String -> Maybe SemExpression
lookupDefinitionName prog name = definition >>= getExpression
  where
    definition :: Maybe SemProgramPart
    definition =
      find
        ( \case
            SemDeclaration -> False
            SemDefinition (Identifier _ name') _ -> name' == name
        )
        prog
    getExpression :: SemProgramPart -> Maybe SemExpression
    getExpression (SemDefinition _ expr) = Just expr
    getExpression _ = Nothing

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

maybeToEither :: Maybe a -> e -> Either e a
maybeToEither Nothing e = Left e
maybeToEither (Just a) _ = Right a

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
normalizeToAlpha rexp = snd $ runState (normalizeToAlphaS rexp) ([], 0)

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
