module Errors
  ( LexicalError,
    SemanticError,
    TypeError,
    LexicalElement (..),
    LexicalErrorType (..),
    SemanticErrorType (..),
    TypeErrorType (..),
    CompilerErrorType (..),
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
    incTextPos,
  )
where

import qualified Data.List as Li

type LineNum = Int

type ColNum = Int

type TextPos = (LineNum, ColNum)

data ProgramError e = ProgramError e TextPos deriving (Eq)

type LexicalError = ProgramError LexicalErrorType

type SemanticError = ProgramError SemanticErrorType

type TypeError = ProgramError TypeErrorType

type CompilerError = ProgramError CompilerErrorType

data CompilerErrorType
  = CeLexicalErrorType LexicalErrorType
  | CeSemanticErrorType SemanticErrorType
  | CeTypeErrorType TypeErrorType
  deriving Eq

instance Show CompilerErrorType where
    show (CeLexicalErrorType t) = "lexical error: " ++ show t
    show (CeSemanticErrorType t) = "semantic error: " ++ show t
    show (CeTypeErrorType t) = "type error: " ++ show t

data LexicalErrorType
  = LeUnexpectedLexicalElement [LexicalElement]
  | LeUnknownTypePostfix
  | LeUnknownTypeName
  deriving (Eq)

instance (Show e) => Show (ProgramError e) where
  show (ProgramError e (line, col)) =
    "Error on line ("
      ++ show line
      ++ ","
      ++ show col
      ++ "): "
      ++ show e

instance Show LexicalErrorType where
  show (LeUnexpectedLexicalElement expected) =
    "Unexpected lexical element. Expected elements: "
      ++ Li.intercalate ", " (map show expected)
  show LeUnknownTypePostfix = "Unknown type postfix."
  show LeUnknownTypeName = "Unknown type name."

data TypeErrorType
  = TeReferenceNotFound
  | TeSelfReferenceFound Int String
  | TeAtomicTypeMismatch String String
  | TeTypeMismatch String String
  | TeCheckError String String
  | TeApplyingToANonFunction String
  | TeIfThenElseConditionIsNotBool
  | TeMainFunctionIsNotByte
  deriving (Eq)

instance Show TypeErrorType where
  show TeReferenceNotFound = "The reference was not found"
  show (TeSelfReferenceFound _ typ) =
    "Self reference found: "
      ++ typ
  show (TeAtomicTypeMismatch at bt) =
    "Atomic type don't match: "
      ++ at
      ++ ", "
      ++ bt
  show (TeTypeMismatch t1 t2) =
    "Types couldn't be reconciled: "
      ++ t1
      ++ ", "
      ++ t2
  show (TeCheckError t1 t2) =
    "Type wish couldn't be applied: "
      ++ t1
      ++ ", "
      ++ t2
  show (TeApplyingToANonFunction t2) =
    "Trying to apply to a non-function: " ++ t2
  show TeIfThenElseConditionIsNotBool = 
    "The condition in the if then else expression is not bool"
  show TeMainFunctionIsNotByte =
    "The main function is not U8"

data SemanticErrorType
  = SeValueRedefinition
  | SeUndefinedVariable String
  | SeTypeRedeclaration
  deriving (Eq)

instance Show SemanticErrorType where
  show SeValueRedefinition = "Value has been redefined."
  show (SeUndefinedVariable name) = "Value is undefined: " ++ name
  show SeTypeRedeclaration = "The type has been redeclared."

data LexicalElement
  = LeColon
  | LeStar
  | LePlus
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
  | LeLiteral
  | LeOpeningBracket
  | LeClosingBracket
  | LeOpeningSquareBracket
  | LeClosingSquareBracket
  | LeEndOfFile
  deriving (Eq)

instance Show LexicalElement where
  show LeColon = "colon (:)"
  show LeStar = "star (*)"
  show LePlus = "plus (+)"
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
  show LeLiteral = "a literal (eg. 42i32)"
  show LeOpeningBracket = "opening bracket"
  show LeClosingBracket = "closing bracket"
  show LeOpeningSquareBracket = "opening square bracket"
  show LeClosingSquareBracket = "closing square bracket"
  show LeEndOfFile = "end of file"

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
incTextPos '\n' (x, _) = (x + 1, 1)
incTextPos _ (x, y) = (x, y + 1)
