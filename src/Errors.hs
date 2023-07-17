module Errors
  ( CompilerError (..),
    LexicalError (..),
    SemanticError (..),
    RuntimeError (..),
    RunErrorable,
    CompileErrorable,
    RunAndCompileErrorable,
  )
where

data CompilerError
  = CompilerError
  | LexicalError LexicalError
  | SemanticError SemanticError
  deriving (Eq, Show)

data LexicalError = UnexpectedEndOfFile | LambdaExpressionExpected | InvalidLambdaExpression | EndOfFileExpected | OtherError | CaseError deriving (Show, Eq) -- Todo: remove OtherError

data SemanticError = ValueRedefinition | SUndefinedVariable String deriving (Show, Eq)

data RuntimeError = ComputationLimitReached | TypeError String | RUndefinedReference String
  deriving (Eq, Show)

type RunErrorable a = Either RuntimeError a

type CompileErrorable a = Either CompilerError a

type RunAndCompileErrorable a = Either (Either CompilerError RuntimeError) a
