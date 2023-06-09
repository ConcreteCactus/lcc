{-# OPTIONS_GHC -Wincomplete-patterns #-}

module Lexer
  ( LexicalError,
    Parser,
    lambda,
    dot,
    openParen,
    closeParen,
    identifier,
  )
where

import Control.Applicative
import Control.Monad
import Data.Char

data LexicalError = UnexpectedEndOfFile | LambdaExpressionExpected | InvalidLambdaExpression | OtherError deriving (Show) -- Todo: remove OtherError

newtype Parser t = Parser {runParser :: String -> Either LexicalError (t, String)} deriving (Functor)

instance Applicative Parser where
  pure :: a -> Parser a
  pure a = Parser (\s -> Right (a, s))

  (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  f <*> a = do
    x <- f
    x <$> a

instance Alternative Parser where
  empty :: Parser a
  empty = Parser $ const $ Left OtherError

  (<|>) :: Parser a -> Parser a -> Parser a
  (Parser a) <|> (Parser b) = Parser (\s -> a s `or'` b s)
    where
      or' :: Either LexicalError a -> Either LexicalError a -> Either LexicalError a
      or' (Right v) _ = Right v
      or' (Left _) e = e

instance Monad Parser where
  (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  (Parser a) >>= f = Parser (a >=> (\(b, sb) -> runParser (f b) sb))

satisfyE :: LexicalError -> (Char -> Bool) -> Parser Char
satisfyE e p = Parser parse
  where
    parse :: String -> Either LexicalError (Char, String)
    parse [] = Left UnexpectedEndOfFile
    parse (c : cs) = if p c then Right (c, cs) else Left e

charE :: LexicalError -> Char -> Parser Char
charE e c = satisfyE e (== c)

-- stringE :: LexicalError -> String -> Parser String
-- stringE e = mapM (charE e)

lambda :: Parser ()
lambda = void (charE LambdaExpressionExpected '\\')

dot :: Parser ()
dot = void (charE InvalidLambdaExpression '.')

openParen :: Parser ()
openParen = void (charE OtherError '(')

closeParen :: Parser ()
closeParen = void (charE OtherError ')')

identifier :: Parser String
identifier = some $ satisfyE OtherError isAlpha
