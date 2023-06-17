{-# OPTIONS_GHC -Wincomplete-patterns #-}

module Lexer
  ( CompilerError (..),
    Parser,
    runParser,
    whiteSpace,
    whiteSpaceO,
    lambda,
    dot,
    openParen,
    closeParen,
    identifier,
    arrow,
    colon,
    colonEquals,
    sepBy1,
    sepBy,
    endOfLine,
    endOfLineO,
    eof,
    (<|>),
  )
where

import Control.Applicative
import Control.Monad
import Data.Bifunctor
import Data.Char
import Errors

newtype Parser t = Parser {runParser :: String -> Either CompilerError (t, String)}

instance Functor Parser where
  fmap f (Parser a) = Parser $ fmap (first f) . a

instance Applicative Parser where
  pure a = Parser (\s -> Right (a, s))
  f <*> a = do
    x <- f
    x <$> a

instance Alternative Parser where
  empty = Parser $ const $ Left CompilerError
  (Parser a) <|> (Parser b) = Parser (\s -> a s `or'` b s)
    where
      or' :: Either CompilerError a -> Either CompilerError a -> Either CompilerError a
      or' (Right v) _ = Right v
      or' (Left _) e = e

instance Monad Parser where
  (Parser a) >>= f = Parser (a >=> (\(b, sb) -> runParser (f b) sb))

satisfyE :: CompilerError -> (Char -> Bool) -> Parser Char
satisfyE e p = Parser parse
  where
    parse :: String -> Either CompilerError (Char, String)
    parse [] = Left $ LexicalError UnexpectedEndOfFile
    parse (c : cs) = if p c then Right (c, cs) else Left e

charE :: CompilerError -> Char -> Parser Char
charE e c = satisfyE e (== c)

stringE :: CompilerError -> String -> Parser String
stringE e = mapM (charE e)

sepBy1 :: Parser () -> Parser a -> Parser [a]
sepBy1 separator token = (:) <$> token <*> many (separator *> token)

sepBy :: Parser () -> Parser a -> Parser [a]
sepBy separator token = sepBy1 separator token <|> (nothing *> pure [])

isSimpleSpace :: Char -> Bool
isSimpleSpace '\n' = False
isSimpleSpace s = isSpace s

whiteSpace :: Parser ()
whiteSpace = void $ some (many (charE CompilerError '\n') *> some (satisfyE CompilerError isSimpleSpace))

nothing :: Parser ()
nothing = Parser (\s -> Right ((), s))

eof :: Parser ()
eof = Parser (\s -> if s == "" then Right ((), "") else Left (LexicalError EndOfFileExpected))

whiteSpaceO :: Parser ()
whiteSpaceO = whiteSpace <|> nothing

endOfLine :: Parser ()
endOfLine = void (charE CompilerError '\n') <|> void (stringE CompilerError "\r\n")

endOfLineO :: Parser ()
endOfLineO = endOfLine <|> nothing

lambda :: Parser ()
lambda = void $ charE (LexicalError LambdaExpressionExpected) '\\'

dot :: Parser ()
dot = void $ charE (LexicalError InvalidLambdaExpression) '.'

openParen :: Parser ()
openParen = void $ charE CompilerError '('

closeParen :: Parser ()
closeParen = void $ charE CompilerError ')'

identifier :: Parser String
identifier = some $ satisfyE CompilerError isAlphaNum

arrow :: Parser ()
arrow = void $ stringE CompilerError "->"

colon :: Parser ()
colon = void $ charE CompilerError ':'

colonEquals :: Parser ()
colonEquals = void $ stringE CompilerError ":="

-- Unit tests

tests :: [Bool]
tests =
  [ runParser whiteSpace " " == Right ((), ""),
    runParser whiteSpace "  " == Right ((), ""),
    runParser whiteSpace "\t \ta" == Right ((), "a"),
    runParser whiteSpace "  a" == Right ((), "a"),
    runParser whiteSpace "  \t\n hello" == Right ((), "hello"),
    runParser whiteSpace " \t \n" == Right ((), "\n"),
    runParser whiteSpace " \t \nhello" == Right ((), "\nhello"),
    runParser whiteSpace "\nhello" == Left CompilerError
  ]
