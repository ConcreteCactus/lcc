{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module SyntacticAnalyzer.Internal where

import Control.Applicative
import Control.Monad
import qualified Errors as E
import qualified Lexer as L
import Util

data Literal = Literal AtomicType Integer deriving (Eq)

instance Show Literal where
  show (Literal _ n) = show n

data Expression
  = Id L.VarIdent
  | Lit Literal
  | Lambda L.VarIdent Expression
  | Application Expression Expression
  | IfThenElse Expression Expression Expression
  deriving (Show, Eq)

{- FOURMOLU_DISABLE -}
data AtomicType
  = AI8 | AI16 | AI32 | AI64 | AI128
  | AU8 | AU16 | AU32 | AU64 | AU128 | AUSize
  | AF32 | AF64 | AChar | ABool
  deriving (Show, Eq)
{- FOURMOLU_ENABLE -}

data Type
  = TypeId L.TypIdent
  | TypeName AtomicType
  | FunctionType Type Type
  | SumType Type Type
  | ProductType Type Type
  | ListType Type
  deriving (Show, Eq)

data ProgramPart
  = Definition E.TextPos L.VarIdent Expression
  | Declaration E.TextPos L.VarIdent Type
  deriving (Eq, Show)

type Program = [ProgramPart]

getTypeFromStr :: String -> Maybe AtomicType
getTypeFromStr name
  | name == "i8" = Just AI8
  | name == "i16" = Just AI16
  | name == "i32" = Just AI32
  | name == "i64" = Just AI64
  | name == "i128" = Just AI128
  | name == "u8" = Just AU8
  | name == "u16" = Just AU16
  | name == "u32" = Just AU32
  | name == "u64" = Just AU64
  | name == "u128" = Just AU128
  | name == "f32" = Just AF32
  | name == "f64" = Just AF64
  | name == "char" = Just AChar
  | name == "bool" = Just ABool
  | otherwise = Nothing

getTypeNameFromStr :: String -> Maybe AtomicType
getTypeNameFromStr name
  | name == "I8" = Just AI8
  | name == "I16" = Just AI16
  | name == "I32" = Just AI32
  | name == "I64" = Just AI64
  | name == "I128" = Just AI128
  | name == "U8" = Just AU8
  | name == "U16" = Just AU16
  | name == "U32" = Just AU32
  | name == "U64" = Just AU64
  | name == "U128" = Just AU128
  | name == "F32" = Just AF32
  | name == "F64" = Just AF64
  | name == "Char" = Just AChar
  | name == "Bool" = Just ABool
  | otherwise = Nothing

optional_ :: L.ParserE a -> L.ParserE ()
optional_ = void . optional

-- Optional Expression Whitespace
wo :: L.ParserE ()
wo = void $ optional L.exprWhiteSpace

-- Required Expression Whitespace
wr :: L.ParserE ()
wr = L.exprWhiteSpace

literal :: L.ParserE Expression
literal =
  Lit
    <$> L.collapseEither
      ( ( \(L.Literal val typs) ->
            ( (`Literal` val)
                <$> getTypeFromStr typs
            )
              `maybeToEither` E.LeUnknownTypePostfix
        )
          <$> L.literal
      )

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
  TypeName
    <$> L.collapseEither
      ( (`maybeToEither` E.LeUnknownTypeName)
          . (\(L.TypName n) -> getTypeNameFromStr n)
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
