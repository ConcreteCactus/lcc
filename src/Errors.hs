module Errors
  ( LexicalError,
    SemanticError,
    TypeError,
    LexicalElement(..),
    LexicalErrorType(..),
    SemanticErrorType(..),
    TypeErrorType(..),
    CompilerErrorType(..),
    CompilerError,
    LineNum,
    ColNum,
    TextPos,
    mkLexErr,
    mkLexErr',
    mkSemErr,
    mkTypErr,
    mkCompErrLex,
    mkCompErrSem,
    mkCompErrTyp,
    incTextPos
  )
where

import qualified Data.List as Li

type LineNum = Int

type ColNum = Int

type TextPos = (LineNum, ColNum)

data ProgramError e = ProgramError e TextPos deriving (Show, Eq)

type LexicalError = ProgramError LexicalErrorType

type SemanticError = ProgramError SemanticErrorType

type TypeError = ProgramError TypeErrorType

type CompilerError = ProgramError CompilerErrorType

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
  | TeIfThenElseConditionIsNotBool
  deriving (Show, Eq)

data SemanticErrorType
  = SeValueRedefinition
  | SeUndefinedVariable String
  | SeTypeRedeclaration
  deriving (Show, Eq)

data LexicalElement
  -- = LeChar Char
  -- | LeWord String
  -- | LeOperator String
  -- | LeWhiteSpace
  -- | LeNotWhiteSpace
  -- | LeEndOfStatement
  -- | LeEndOfLine
  -- | LeLowercaseCharacter
  -- | LeIdentFirstCharacter
  -- | LeIdentCharacter
  -- | LeUppercaseCharacter
  -- | LeAlphabeticalCharacter
  -- | LeAlphanumericCharacter
  -- | LeNumber
  -- | LeDigit
  -- | LeHexDigit
  -- | LeNotNewLine
  -- | LeTypeName
  = LeColon
  | LeArrow
  | LeColonEquals
  | LeBackslash
  | LeDot
  | LeExprWhiteSpace
  | LeStmtWhiteSpace
  | LeIf
  | LeThen
  | LeElse
  | LeVarIdent
  | LeTypIdent
  deriving (Eq)

instance Show LexicalElement where
    show LeColon = ":"
    show LeArrow = "->"
    show LeColonEquals = ":="
    show LeBackslash = "\\"
    show LeDot = "."
    show LeExprWhiteSpace = "white space"
    show LeStmtWhiteSpace = "white space with a following new statement"
    show LeIf = "if"
    show LeThen = "then"
    show LeElse = "else"
    show LeVarIdent = "variable name (eg. x)"
    show LeTypIdent = "type name (eg. I32)"

mkLexErr :: TextPos -> [LexicalElement] -> LexicalError
mkLexErr pos expect = ProgramError (LeUnexpectedLexicalElement expect) pos

mkLexErr' :: TextPos -> LexicalErrorType -> LexicalError
mkLexErr' pos typ = ProgramError typ pos

mkSemErr :: TextPos -> SemanticErrorType -> SemanticError
mkSemErr pos typ = ProgramError typ pos

mkTypErr :: TextPos -> TypeErrorType -> TypeError
mkTypErr pos typ = ProgramError typ pos

mkCompErrLex :: LexicalError -> CompilerError
mkCompErrLex (ProgramError et pos) = ProgramError (CeLexicalErrorType et) pos
mkCompErrSem :: SemanticError -> CompilerError
mkCompErrSem (ProgramError et pos) = ProgramError (CeSemanticErrorType et) pos
mkCompErrTyp :: TypeError -> CompilerError
mkCompErrTyp (ProgramError et pos) = ProgramError (CeTypeErrorType et) pos

incTextPos :: Char -> TextPos -> TextPos
incTextPos '\n' (x, _) = (x + 1, 0)
incTextPos _ (x, y) = (x, y + 1)
