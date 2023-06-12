{-# OPTIONS_GHC -Wincomplete-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use tuple-section" #-}
{-# HLINT ignore "Use execState" #-}

module SemanticAnalyzer
  ( SemanticError (..),
    Identifier (..),
    SemExpression (..),
    createSemanticExpression,
  )
where

import Data.Foldable
import Errors
import SyntacticAnalyzer

newtype State s a = State {runState :: s -> (s, a)}

instance Functor (State s) where
  fmap f (State a) = State $ fmap f . a

instance Applicative (State s) where
  pure a = State (\s -> (s, a))
  (State sab) <*> (State sa) =
    State
      ( \s1 ->
          let (s2, ab) = sab s1
           in let (s3, a) = sa s2 in (s3, ab a)
      )

instance Monad (State s) where
  return = pure
  (State sa) >>= f =
    State
      ( \s1 ->
          let (s2, a) = sa s1
           in let (State sb) = f a in sb s2
      )

get :: State s s
get = State (\s -> (s, s))

put :: s -> State s ()
put ns = State $ const (ns, ())

data SemanticError = UndefinedVariable
  deriving (Show)

data Identifier = Identifier Integer String deriving (Show, Eq)

data SemExpression
  = SId Identifier
  | SLambda Identifier SemExpression
  | SApplication SemExpression SemExpression
  deriving (Eq, Show)

data AnalyzerState = AnalyzerState {variables :: [Identifier], variableCounter :: Integer}

startingAnalyzerState :: AnalyzerState
startingAnalyzerState = AnalyzerState [] 0

getVars :: State AnalyzerState [Identifier]
getVars = variables <$> get

getCount :: State AnalyzerState Integer
getCount = variableCounter <$> get

pushVar :: Identifier -> State AnalyzerState ()
pushVar ident = do
  AnalyzerState vars count <- get
  put $ AnalyzerState (ident : vars) count

popVar :: State AnalyzerState ()
popVar = do
  AnalyzerState vars count <- get
  put $ AnalyzerState (tail vars) count

incCount :: State AnalyzerState ()
incCount = do
  AnalyzerState vars count <- get
  put $ AnalyzerState vars $ count + 1

createSemanticExpressionS ::
  SynExpression -> State AnalyzerState (Either CompilerError SemExpression)
createSemanticExpressionS (Id ident) = do
  vars <- getVars
  case find (\(Identifier _ name) -> name == ident) vars of
    Just identifier -> return $ Right $ SId identifier
    Nothing -> do
      count <- getCount
      incCount
      return $ Right $ SId $ Identifier (count + 1) ident
createSemanticExpressionS (Lambda param expr) = do
  count <- getCount
  let paramIdentifier = Identifier (count + 1) param
  pushVar paramIdentifier
  incCount
  sformula <- createSemanticExpressionS expr
  popVar
  return $ SLambda paramIdentifier <$> sformula
createSemanticExpressionS (Application func arg) = do
  sfunc <- createSemanticExpressionS func
  sarg <- createSemanticExpressionS arg
  return $ SApplication <$> sfunc <*> sarg

createSemanticExpression :: SynExpression -> Either CompilerError SemExpression
createSemanticExpression expr = snd $ runState (createSemanticExpressionS expr) startingAnalyzerState

-- Unit tests

test :: String -> Either CompilerError SemExpression
test s = parseExpression s >>= createSemanticExpression

tests :: [Bool]
tests =
  [ test "\\a.b" == Right (SLambda (Identifier 1 "a") (SId $ Identifier 2 "b")),
    test "\\a.\\b.a b" == Right (SLambda (Identifier 1 "a") (SLambda (Identifier 2 "b") (SApplication (SId $ Identifier 1 "a") (SId $ Identifier 2 "b")))),
    test "(\\a.a) x" == Right (SApplication (SLambda (Identifier 1 "a") (SId $ Identifier 1 "a")) (SId (Identifier 2 "x")))
  ]
