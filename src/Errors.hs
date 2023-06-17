module Errors
  ( CompilerError (..),
    LexicalError (..),
  )
where

data CompilerError
  = CompilerError
  | LexicalError LexicalError
  deriving (Eq, Show)

data LexicalError = UnexpectedEndOfFile | LambdaExpressionExpected | InvalidLambdaExpression | EndOfFileExpected | OtherError deriving (Show, Eq) -- Todo: remove OtherError
