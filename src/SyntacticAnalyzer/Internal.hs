{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module SyntacticAnalyzer.Internal where

import Control.Applicative
import Control.Monad
import AtomicType
import qualified Errors as E
import qualified Lexer as L
import Util

data Expression
  = Id L.VarIdent
  | Lit L.Literal
  | Lambda L.VarIdent Expression
  | Application Expression Expression
  | IfThenElse Expression Expression Expression
  deriving (Show, Eq)

data Type
  = TypeId L.TypIdent
  | TypeName AtomicType
  | FunctionType Type Type
  | SumType Type Type
  | ProductType Type Type
  | ListType Type
  | UnitType
  | EmptyType
  deriving (Show, Eq)

data ProgramPart
  = Definition E.TextPos L.VarIdent Expression
  | Declaration E.TextPos L.VarIdent Type
  deriving (Eq, Show)

type Program = [ProgramPart]

getTypeFromStr :: String -> Maybe Type
getTypeFromStr name =
  (if name == "Unit" then Just UnitType else Nothing)
    <|> (if name == "Empty" then Just EmptyType else Nothing)
    <|> (TypeName <$> getTypeNameFromStr name)

optional_ :: L.ParserE a -> L.ParserE ()
optional_ = void . optional

-- Optional Expression Whitespace
wo :: L.ParserE ()
wo = void $ optional L.exprWhiteSpace

-- Required Expression Whitespace
wr :: L.ParserE ()
wr = L.exprWhiteSpace

literal :: L.ParserE Expression
literal = Lit <$> L.literal

varIdent :: L.ParserE Expression
varIdent = Id <$> L.varIdent

lambda :: L.ParserE Expression
lambda =
  Lambda
    <$> (wo *> L.backSlash *> wo *> L.varIdent)
    <*> (wo *> L.dot *> wo *> expression)

applications :: L.ParserE Expression
applications = wo *> L.leftAssoc2 Application wr expressionWoA

ifThenElse :: L.ParserE Expression
ifThenElse =
  IfThenElse
    <$> (wo *> L.if_ *> wr *> expression)
    <*> (wr *> L.then_ *> wr *> expression)
    <*> (wr *> L.else_ *> wr *> expression)

expression :: L.ParserE Expression
expression =
  ifThenElse
    <|> lambda
    <|> applications
    <|> expressionBr
    <|> varIdent
    <|> literal

-- Expression inside brackets
expressionBr :: L.ParserE Expression
expressionBr =
  wo
    *> L.openingBracket
    *> wo
    *> expression
    <* wo
    <* L.closingBracket

-- Expression without application
expressionWoA :: L.ParserE Expression
expressionWoA =
  ifThenElse
    <|> lambda
    <|> expressionBr
    <|> varIdent
    <|> literal

typeId :: L.ParserE Type
typeId = TypeId <$> L.typIdent

typeName :: L.ParserE Type
typeName =
  L.collapseEither
    ( (`maybeToEither` E.LeUnknownTypeName)
        . (\(L.TypName n) -> getTypeFromStr n)
        <$> L.typName
    )

functionType :: L.ParserE Type
functionType =
  FunctionType
    <$> (wo *> typeWoF)
    <*> (wo *> L.arrow *> wo *> type_)

sumType :: L.ParserE Type
sumType =
  SumType
    <$> (wo *> typeWoS)
    <*> (wo *> L.plus *> wo *> typeWoF)

productType :: L.ParserE Type
productType =
  ProductType
    <$> (wo *> typeWoT)
    <*> (wo *> L.star *> wo *> typeWoS)

listType :: L.ParserE Type
listType =
  ListType
    <$> ( wo
            *> L.openingSquareBracket
            *> type_
            <* L.closingSquareBracket
        )

type_ :: L.ParserE Type
type_ =
  functionType
    <|> sumType
    <|> productType
    <|> listType
    <|> typeId
    <|> typeName
    <|> typeBr

-- A type inside brackets
typeBr :: L.ParserE Type
typeBr = wo *> L.openingBracket *> wo *> type_ <* wo <* L.closingBracket

-- Type without FunctionType
typeWoF :: L.ParserE Type
typeWoF =
  sumType
    <|> productType
    <|> listType
    <|> typeId
    <|> typeName
    <|> typeBr

-- Type without SumType and FuncionType
typeWoS :: L.ParserE Type
typeWoS =
  productType
    <|> listType
    <|> typeId
    <|> typeName
    <|> typeBr

-- Type without ProductType, SumType, and FunctionType
typeWoT :: L.ParserE Type
typeWoT =
  listType
    <|> typeId
    <|> typeName
    <|> typeBr

definition :: L.ParserE ProgramPart
definition =
  Definition
    <$> L.currentPos
    <*> L.varIdent
    <*> (wo *> L.colonEquals *> wo *> expression)

declaration :: L.ParserE ProgramPart
declaration =
  Declaration
    <$> L.currentPos
    <*> L.varIdent
    <*> (wo *> L.colon *> wo *> type_)

programPart :: L.ParserE ProgramPart
programPart = definition <|> declaration

program :: L.ParserE Program
program = L.sepBy L.stmtWhiteSpace programPart <* L.endOfFile

parseProgram :: L.SourceCode -> Either E.LexicalError Program
parseProgram = L.execParser program
