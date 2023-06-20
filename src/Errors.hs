module Errors
  ( CompilerError (..),
    LexicalError (..),
    SemanticError (..),
  )
where

data CompilerError
  = CompilerError
  | LexicalError LexicalError
  | SemanticError SemanticError
  deriving (Eq, Show)

data LexicalError = UnexpectedEndOfFile | LambdaExpressionExpected | InvalidLambdaExpression | EndOfFileExpected | OtherError | CaseError deriving (Show, Eq) -- Todo: remove OtherError

data SemanticError = ValueRedefinition | UndefinedVariable deriving (Show, Eq)
