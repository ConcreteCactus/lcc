module Errors
  ( CompilerError (..),
    LexicalError (..),
  )
where

data CompilerError
  = CompilerError
  | LexicalError LexicalError
  deriving (Eq, Show)

data LexicalError = UnexpectedEndOfFile | LambdaExpressionExpected | InvalidLambdaExpression | OtherError deriving (Show, Eq) -- Todo: remove OtherError
