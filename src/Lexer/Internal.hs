{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Lexer.Internal where

import Control.Applicative
import Errors
import Control.Monad

type SourceCode = String

newtype Parser a
  = Parser
      ((TextPos, SourceCode) -> Maybe (TextPos, a, SourceCode))

newtype ParserE a
  = ParserE
      ( (TextPos, SourceCode) ->
        Either LexicalError (TextPos, a, SourceCode)
      )

instance Functor Parser where
  fmap f (Parser fa) =
    Parser
      ( \(textPos, sourceCode) -> do
          (textPos', parsedValue, sourceCode') <- fa (textPos, sourceCode)
          return (textPos', f parsedValue, sourceCode')
      )

instance Applicative Parser where
  pure a = Parser $ const $ pure ((0, 0), a, "")
  (Parser ff) <*> (Parser fa) =
    Parser
      ( \(textPos, sourceCode) -> do
          (textPos', f, sourceCode') <- ff (textPos, sourceCode)
          (textPos'', parsedValue, sourceCode'') <- fa (textPos', sourceCode')
          return (textPos'', f parsedValue, sourceCode'')
      )

instance Alternative Parser where
  (Parser fa) <|> (Parser fb) =
    Parser
      ( \(textPos, sourceCode) -> fa (textPos, sourceCode) <|> fb (textPos, sourceCode)
      )
  empty = Parser (const Nothing)

instance Monad Parser where
  (Parser fa) >>= fp =
    Parser
      ( \(textPos, sourceCode) -> do
          (textPos', parsedValue, sourceCode') <- fa (textPos, sourceCode)
          let (Parser fa') = fp parsedValue
          fa' (textPos', sourceCode')
      )

instance Functor ParserE where
  fmap f (ParserE fa) =
    ParserE
      ( \(textPos, sourceCode) -> do
          (textPos', parsedValue, sourceCode') <- fa (textPos, sourceCode)
          return (textPos', f parsedValue, sourceCode')
      )

instance Applicative ParserE where
  pure a = ParserE $ const $ pure ((0, 0), a, "")
  (ParserE ff) <*> (ParserE fa) =
    ParserE
      ( \(textPos, sourceCode) -> do
          (textPos', f, sourceCode') <- ff (textPos, sourceCode)
          (textPos'', parsedValue, sourceCode'') <- fa (textPos', sourceCode')
          return (textPos'', f parsedValue, sourceCode'')
      )

instance Alternative ParserE where
  (ParserE fa) <|> (ParserE fb) =
    ParserE
      ( \(textPos, sourceCode) -> case fa (textPos, sourceCode) of
          Right a -> Right a
          Left _ -> fb (textPos, sourceCode)
      )
  empty =
    ParserE
      $ const
      $ Left
      $ mkLexErr' (0, 0)
      $ LeUnexpectedLexicalElement []

instance Monad ParserE where
  (ParserE fa) >>= fp =
    ParserE
      ( \(textPos, sourceCode) -> do
          (textPos', parsedValue, sourceCode') <- fa (textPos, sourceCode)
          let (ParserE fa') = fp parsedValue
          fa' (textPos', sourceCode')
      )

satisfy :: (Char -> Bool) -> Parser Char
satisfy f =
  Parser
    ( \(textPos, sourceCode) -> case sourceCode of
        (c : cs) | f c -> Just (incTextPos c textPos, c, cs)
        _ -> Nothing
    )

satisfy_ :: (Char -> Bool) -> Parser ()
satisfy_ f = void $ satisfy f

withError :: Parser a -> LexicalErrorType -> ParserE a
withError (Parser fa) e =
  ParserE
    ( \(textPos, sourceCode) ->
        case fa (textPos, sourceCode) of
          Just (textPos', parsedValue, sourceCode') ->
            Right (textPos', parsedValue, sourceCode')
          Nothing -> Left $ mkLexErr' textPos e
    )

infixl 4 `withError`

withExpected :: Parser a -> LexicalElement -> ParserE a
withExpected p l = p `withError` LeUnexpectedLexicalElement [l]

infixl 4 `withExpected`

colon :: ParserE ()
colon = satisfy_ (==':') `withExpected` LeColon

arrow :: ParserE ()
arrow = satisfy_ (=='-') *> satisfy_ (=='>') `withExpected` LeColon

colonEquals :: ParserE ()
colonEquals = satisfy_ (==':') *> satisfy_ (=='=') `withExpected` LeColonEquals

backSlash :: ParserE ()
backSlash = satisfy_ (=='\\') `withExpected` LeBackslash

dot :: ParserE ()
dot = satisfy_ (=='.') `withExpected` LeBackslash


