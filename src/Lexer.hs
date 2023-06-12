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
    sepBy1,
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

-- stringE :: CompilerError -> String -> Parser String
-- stringE e = mapM (charE e)

sepBy1 :: Parser () -> Parser a -> Parser [a]
sepBy1 separator token = (:) <$> token <*> many (separator *> token)

whiteSpace :: Parser ()
whiteSpace = void $ some $ satisfyE CompilerError isSpace

nothing :: Parser ()
nothing = Parser (\s -> Right ((), s))

whiteSpaceO :: Parser ()
whiteSpaceO = whiteSpace <|> nothing

lambda :: Parser ()
lambda = void (charE (LexicalError LambdaExpressionExpected) '\\')

dot :: Parser ()
dot = void (charE (LexicalError InvalidLambdaExpression) '.')

openParen :: Parser ()
openParen = void (charE CompilerError '(')

closeParen :: Parser ()
closeParen = void (charE CompilerError ')')

identifier :: Parser String
identifier = some (satisfyE CompilerError isAlpha)
