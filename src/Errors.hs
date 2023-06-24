module Errors
  ( CompilerError (..),
    LexicalError (..),
    SemanticError (..),
    RuntimeError (..),
  )
where

data CompilerError
  = CompilerError
  | LexicalError LexicalError
  | SemanticError SemanticError
  deriving (Eq, Show)

data LexicalError = UnexpectedEndOfFile | LambdaExpressionExpected | InvalidLambdaExpression | EndOfFileExpected | OtherError | CaseError deriving (Show, Eq) -- Todo: remove OtherError

data SemanticError = ValueRedefinition | UndefinedVariable String deriving (Show, Eq)

data RuntimeError = InfiniteLoopError | TypeError | UndefinedVariableError
  deriving (Eq, Show)
