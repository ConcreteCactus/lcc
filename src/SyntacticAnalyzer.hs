{-# OPTIONS_GHC -Wincomplete-patterns #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

module SyntacticAnalyzer
  ( SynExpression (..),
    parseExpression,
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
    runParser expressionParser "(\\a.\\b.b) b c" == Right (Application (Application (Lambda "a" (Lambda "b" (Id "b"))) (Id "b")) (Id "c"), "")
  ]
