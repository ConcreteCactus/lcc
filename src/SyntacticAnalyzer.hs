{-# OPTIONS_GHC -Wincomplete-patterns #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

module SyntacticAnalyzer
  ( Expression (..),
    parseExpression,
  )
where

import Control.Applicative
import Lexer

data Expression
  = Id String
  | Lambda String Expression
  | Application Expression Expression
  deriving (Show, Eq)

parseId :: Parser Expression
parseId = Id <$> identifier

parseLambda :: Parser Expression
parseLambda =
  Lambda
    <$> ( lambda
            *> (whiteSpaceO *> identifier <* whiteSpaceO)
            <* (dot <* whiteSpaceO)
        )
    <*> parseExpression

parseApplications :: Parser Expression
parseApplications = foldl1 Application <$> sepBy1 whiteSpace parseExpressionWithoutApplication

parseExpressionWithoutApplication :: Parser Expression
parseExpressionWithoutApplication =
  parseId
    <|> parseLambda
    <|> (openParen *> parseExpression <* closeParen)

parseExpression :: Parser Expression
parseExpression =
  parseApplications
    <|> parseId
    <|> parseLambda
    <|> ( whiteSpaceO
            *> ( openParen
                   *> (whiteSpaceO *> parseExpression <* whiteSpaceO)
                   <* closeParen
               )
        )

-- Unit tests

tests :: [Bool]
tests =
  -- Id tests
  [ runParser parseExpression "hello" == Right (Id "hello", ""),
    runParser parseExpression "hello   " == Right (Id "hello", "   "),
    runParser parseExpression "   " == Left UnexpectedEndOfFile,
    -- Lambda tests
    runParser parseExpression "\\a.b" == Right (Lambda "a" (Id "b"), ""),
    runParser parseExpression "\\a. b" == Right (Lambda "a" (Id "b"), ""),
    runParser parseExpression "\\a . b" == Right (Lambda "a" (Id "b"), ""),
    runParser parseExpression "\\ a . b" == Right (Lambda "a" (Id "b"), ""),
    runParser parseExpression "\\ a.b" == Right (Lambda "a" (Id "b"), ""),
    runParser parseExpression "\\a .b" == Right (Lambda "a" (Id "b"), ""),
    runParser parseExpression "\\a.b  " == Right (Lambda "a" (Id "b"), "  "),
    runParser parseExpression "\\a.b  b" == Right (Lambda "a" (Application (Id "b") (Id "b")), ""),
    -- Application tests
    runParser parseExpression "a  b" == Right (Application (Id "a") (Id "b"), ""),
    runParser parseExpression "(\\a.b)  b" == Right (Application (Lambda "a" (Id "b")) (Id "b"), ""),
    runParser parseExpression "a b" == Right (Application (Id "a") (Id "b"), ""),
    runParser parseExpression "a b c" == Right (Application (Application (Id "a") (Id "b")) (Id "c"), ""),
    runParser parseExpression "a b c d" == Right (Application (Application (Application (Id "a") (Id "b")) (Id "c")) (Id "d"), ""),
    runParser parseExpression "(\\a.a) b c" == Right (Application (Application (Lambda "a" (Id "a")) (Id "b")) (Id "c"), ""),
    runParser parseExpression "(\\a.\\b.b) b c" == Right (Application (Application (Lambda "a" (Lambda "b" (Id "b"))) (Id "b")) (Id "c"), "")
  ]
