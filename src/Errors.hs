module Errors
  ( LexicalError,
    SemanticError,
    TypeError,
    LexicalElement(..),
    LexicalErrorType(..),
    SemanticErrorType(..),
    TypeErrorType(..),
    LineNum,
    ColNum,
    TextPos,
    mkLexErr,
    mkSemErr,
    mkTypErr,
    incTextPos
  )
where

import qualified Data.List as Li

type LineNum = Int

type ColNum = Int

type TextPos = (LineNum, ColNum)

data ProgramError e = ProgramError e TextPos

type LexicalError = ProgramError LexicalErrorType

type SemanticError = ProgramError SemanticErrorType

type TypeError = ProgramError TypeErrorType

data CompilerErrorType
  = CeLexicalErrorType LexicalErrorType
  | CeSemanticErrorType SemanticErrorType
  | CeTypeErrorType TypeErrorType
  deriving (Eq, Show)

newtype LexicalErrorType
  = LeUnexpectedLexicalElement [LexicalElement]
  deriving (Eq)

instance Show LexicalErrorType where
  show (LeUnexpectedLexicalElement expected) =
    "Unexpected lexical element. Expected elements: "
      ++ Li.intercalate ", " (map show expected)

data TypeErrorType
  = TeReferenceNotFound
  | TeSelfReferenceFound Int String
  | TeAtomicTypeMismatch String String
  | TeTypeMismatch String String
  | TeCheckError String String
  | TeApplyingToANonFunction String
  deriving (Show, Eq)

data SemanticErrorType
  = SeValueRedefinition
  | SeUndefinedVariable String
  | SeTypeRedeclaration
  deriving (Show, Eq)

data LexicalElement
  = LeChar Char
  | LeWord String
  | LeOperator String
  | LeWhiteSpace
  | LeNotWhiteSpace
  | LeEndOfStatement
  | LeEndOfLine
  | LeLowercaseCharacter
  | LeUppercaseCharacter
  | LeAlphabeticalCharacter
  | LeAlphanumericCharacter
  | LeNumber
  | LeDigit
  | LeHexDigit
  | LeNotNewLine
  deriving (Eq)

instance Show LexicalElement where
  show (LeChar c) = show c
  show (LeWord word) = show word
  show (LeOperator op) = show op
  show LeWhiteSpace = "whitespace"
  show LeEndOfLine = "end of line"
  show LeEndOfStatement = "end of statement"
  show LeLowercaseCharacter = "lowercase character"
  show LeUppercaseCharacter = "uppercase character"
  show LeAlphabeticalCharacter = "alphabetic character"
  show LeAlphanumericCharacter = "alphanumeric character"
  show LeNumber = "number"
  show LeDigit = "digit"
  show LeHexDigit = "hexadecimal digit (0-9,a-f,A-F)"
  show LeNotNewLine = "anything but a linefeed"
  show LeNotWhiteSpace = "anything but whitespace"

mkLexErr :: TextPos -> [LexicalElement] -> LexicalError
mkLexErr pos expect = ProgramError (LeUnexpectedLexicalElement expect) pos

mkSemErr :: TextPos -> SemanticErrorType -> SemanticError
mkSemErr pos typ = ProgramError typ pos

mkTypErr :: TextPos -> TypeErrorType -> TypeError
mkTypErr pos typ = ProgramError typ pos

incTextPos :: TextPos -> Char -> TextPos
incTextPos (line, col) c =
  if c == '\n' 
  then (line+1, 1)
  else (line, col+1)
