{-# OPTIONS_GHC -Wincomplete-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use tuple-section" #-}
{-# HLINT ignore "Use execState" #-}

module SemanticAnalyzer
  ( SemanticError,
    createSemanticExpression,
  )
where

import Data.Foldable
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
put ns = State (\_ -> (ns, ()))

data SemanticError = UndefinedVariable
  deriving (Show)

data Identifier = Identifier Integer String deriving (Show)

data SemanticExpression
  = SId Identifier
  | SLambda Identifier SemanticExpression
  | SApplication SemanticExpression SemanticExpression
  deriving (Show)

data AnalyzerState = AnalyzerState {variables :: [Identifier], variableCounter :: Integer}

startingAnalyzerState :: AnalyzerState
startingAnalyzerState = AnalyzerState [] 0

createSemanticExpressionS ::
  Expression -> State AnalyzerState (Either SemanticError SemanticExpression)
createSemanticExpressionS (Id ident) = do
  (AnalyzerState vars _) <- get
  case find (\(Identifier _ name) -> name == ident) vars of
    Just identifier -> return $ Right $ SId identifier
    Nothing -> return $ Left UndefinedVariable
createSemanticExpressionS (Lambda param expr) = do
  (AnalyzerState vars1 count1) <- get
  let paramIdentifier = Identifier (count1 + 1) param
  put $ AnalyzerState (paramIdentifier : vars1) (count1 + 1)
  sformula <- createSemanticExpressionS expr
  (AnalyzerState vars2 count2) <- get
  put $ AnalyzerState (tail vars2) count2
  return $ SLambda paramIdentifier <$> sformula
createSemanticExpressionS (Application func arg) = do
  sfunc <- createSemanticExpressionS func
  sarg <- createSemanticExpressionS arg
  return $ SApplication <$> sfunc <*> sarg

createSemanticExpression :: Expression -> Either SemanticError SemanticExpression
createSemanticExpression expr = snd $ runState (createSemanticExpressionS expr) startingAnalyzerState
