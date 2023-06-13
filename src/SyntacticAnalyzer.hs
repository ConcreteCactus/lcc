{-# OPTIONS_GHC -Wincomplete-patterns #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

module SyntacticAnalyzer
  ( SynExpression (..),
    SynTypeExpression (..),
    parseExpression,
    parseType,
  )
where

import Control.Applicative
import Errors
import Lexer

data SynExpression
  = Id String
  | Lambda String SynExpression
  | Application SynExpression SynExpression
  deriving (Show, Eq)

data SynTypeExpression
  = TypeId String
  | FunctionType SynTypeExpression SynTypeExpression
  deriving (Show, Eq)

data SynProgramPart = SynDefinition String SynExpression | SynDeclaration String SynTypeExpression deriving (Eq, Show)

idParser :: Parser SynExpression
idParser = Id <$> identifier

lambdaParser :: Parser SynExpression
lambdaParser =
  Lambda
    <$> ( lambda
            *> (whiteSpaceO *> identifier <* whiteSpaceO)
            <* (dot <* whiteSpaceO)
        )
    <*> expressionParser

applicationsParser :: Parser SynExpression
applicationsParser = foldl1 Application <$> sepBy1 whiteSpace parseExpressionWithoutApplication

parseExpressionWithoutApplication :: Parser SynExpression
parseExpressionWithoutApplication =
  idParser
    <|> lambdaParser
    <|> (openParen *> expressionParser <* closeParen)

expressionParser :: Parser SynExpression
expressionParser =
  applicationsParser
    <|> idParser
    <|> lambdaParser
    <|> ( whiteSpaceO
            *> ( openParen
                   *> (whiteSpaceO *> expressionParser <* whiteSpaceO)
                   <* closeParen
               )
        )

parseExpression :: String -> Either CompilerError SynExpression
parseExpression s = fst <$> runParser expressionParser s

tIdParser :: Parser SynTypeExpression
tIdParser = TypeId <$> identifier

functionParser :: Parser SynTypeExpression
functionParser = FunctionType <$> typeParserWithoutFunction <*> (whiteSpaceO *> arrow *> whiteSpaceO *> typeParser)

typeParserWithoutFunction :: Parser SynTypeExpression
typeParserWithoutFunction =
  tIdParser
    <|> ( whiteSpaceO
            *> ( openParen
                   *> (whiteSpaceO *> typeParser <* whiteSpaceO)
                   <* closeParen
               )
            <* whiteSpaceO
        )

typeParser :: Parser SynTypeExpression
typeParser =
  functionParser
    <|> tIdParser
    <|> ( whiteSpaceO
            *> ( openParen
                   *> (whiteSpaceO *> typeParser <* whiteSpaceO)
                   <* closeParen
               )
            <* whiteSpaceO
        )

parseType :: String -> Either CompilerError SynTypeExpression
parseType s = fst <$> runParser typeParser s

parseDeclaration :: Parser SynProgramPart
parseDeclaration = SynDeclaration <$> identifier <*> (whiteSpaceO *> colon *> whiteSpaceO *> typeParser)

parseDefinition :: Parser SynProgramPart
parseDefinition = SynDefinition <$> identifier <*> (whiteSpaceO *> colonEquals *> whiteSpaceO *> expressionParser)

-- Unit tests

tests :: [Bool]
tests =
  -- Id tests
  [ runParser expressionParser "hello" == Right (Id "hello", ""),
    runParser expressionParser "hello   " == Right (Id "hello", "   "),
    runParser expressionParser "   " == Left (LexicalError UnexpectedEndOfFile),
    -- Lambda tests
    runParser expressionParser "\\a.b" == Right (Lambda "a" (Id "b"), ""),
    runParser expressionParser "\\a. b" == Right (Lambda "a" (Id "b"), ""),
    runParser expressionParser "\\a . b" == Right (Lambda "a" (Id "b"), ""),
    runParser expressionParser "\\ a . b" == Right (Lambda "a" (Id "b"), ""),
    runParser expressionParser "\\ a.b" == Right (Lambda "a" (Id "b"), ""),
    runParser expressionParser "\\a .b" == Right (Lambda "a" (Id "b"), ""),
    runParser expressionParser "\\a.b  " == Right (Lambda "a" (Id "b"), "  "),
    runParser expressionParser "\\a.b  b" == Right (Lambda "a" (Application (Id "b") (Id "b")), ""),
    -- Application tests
    runParser expressionParser "a  b" == Right (Application (Id "a") (Id "b"), ""),
    runParser expressionParser "(\\a.b)  b" == Right (Application (Lambda "a" (Id "b")) (Id "b"), ""),
    runParser expressionParser "a b" == Right (Application (Id "a") (Id "b"), ""),
    runParser expressionParser "a b c" == Right (Application (Application (Id "a") (Id "b")) (Id "c"), ""),
    runParser expressionParser "a b c d" == Right (Application (Application (Application (Id "a") (Id "b")) (Id "c")) (Id "d"), ""),
    runParser expressionParser "(\\a.a) b c" == Right (Application (Application (Lambda "a" (Id "a")) (Id "b")) (Id "c"), ""),
    runParser expressionParser "(\\a.\\b.b) b c" == Right (Application (Application (Lambda "a" (Lambda "b" (Id "b"))) (Id "b")) (Id "c"), ""),
    -- Type parser tests
    runParser typeParser "a" == Right (TypeId "a", ""),
    runParser typeParser "ab" == Right (TypeId "ab", ""),
    runParser typeParser "ab1  " == Right (TypeId "ab1", "  "),
    runParser typeParser "a -> b" == Right (FunctionType (TypeId "a") (TypeId "b"), ""),
    runParser typeParser "a->b" == Right (FunctionType (TypeId "a") (TypeId "b"), ""),
    runParser typeParser "a -> b -> c" == Right (FunctionType (TypeId "a") (FunctionType (TypeId "b") (TypeId "c")), ""),
    runParser typeParser "(a -> b) -> c" == Right (FunctionType (FunctionType (TypeId "a") (TypeId "b")) (TypeId "c"), "")
  ]
