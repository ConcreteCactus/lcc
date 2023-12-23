{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Lexer.Internal where

import Control.Applicative
import Control.Monad
import Data.Char
import Errors

type SourceCode = String

newtype VarIdent = VarIdent String deriving (Eq)
newtype TypIdent = TypIdent String deriving (Eq)
data Literal = Literal Integer String deriving (Eq)

newtype Parser a
  = Parser
      ((TextPos, SourceCode) -> Maybe (TextPos, a, SourceCode))

newtype ParserE a
  = ParserE
      ( (TextPos, SourceCode) ->
        Either LexicalError (TextPos, a, SourceCode)
      )

instance Show VarIdent where
  show (VarIdent i) = i
instance Show TypIdent where
  show (TypIdent i) = i
instance Show Literal where
  show (Literal n typ) = show n ++ typ

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

collapseMaybe :: Parser (Maybe a) -> Parser a
collapseMaybe (Parser fa) =
  Parser
    ( \a -> case fa a of
        Just (textPos, Just parsedValue, sourceCode) ->
          Just (textPos, parsedValue, sourceCode)
        _ -> Nothing
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
colon = satisfy_ (== ':') `withExpected` LeColon

arrow :: ParserE ()
arrow = satisfy_ (== '-') *> satisfy_ (== '>') `withExpected` LeColon

colonEquals :: ParserE ()
colonEquals = satisfy_ (== ':') *> satisfy_ (== '=') `withExpected` LeColonEquals

backSlash :: ParserE ()
backSlash = satisfy_ (== '\\') `withExpected` LeBackslash

dot :: ParserE ()
dot = satisfy_ (== '.') `withExpected` LeBackslash

exprWhiteSpace' :: Parser ()
exprWhiteSpace' =
  void
    $ some
      ( satisfy_ (`elem` "\t ")
          <|> (satisfy_ (== '\n') *> exprWhiteSpace')
      )

exprWhiteSpace :: ParserE ()
exprWhiteSpace = exprWhiteSpace' `withExpected` LeExprWhiteSpace

stmtWhiteSpace :: ParserE ()
stmtWhiteSpace =
  exprWhiteSpace'
    *> satisfy_ (== '\n')
    `withExpected` LeStmtWhiteSpace

if_ :: ParserE ()
if_ = mapM_ (satisfy . (==)) "if" `withExpected` LeIf

then_ :: ParserE ()
then_ = mapM_ (satisfy . (==)) "then" `withExpected` LeIf

else_ :: ParserE ()
else_ = mapM_ (satisfy . (==)) "else" `withExpected` LeIf

varIdent :: ParserE VarIdent
varIdent =
  VarIdent
    <$> ((:) <$> satisfy isLower <*> some (satisfy isAlphaNum))
    `withExpected` LeVarIdent

typIdent :: ParserE TypIdent
typIdent =
  TypIdent
    <$> ((:) <$> satisfy isAlpha <*> some (satisfy isAlphaNum))
    `withExpected` LeTypIdent

literal :: ParserE Literal
literal = collapseMaybe (decNum <|> hexNum) `withExpected` LeLiteral
 where
  litTyp = some (satisfy (\c -> isAlphaNum c && isLower c))
  decNum =
    convertDec
      <$> optional (satisfy (`elem` "+-"))
      <*> some (satisfy isDigit)
      <*> litTyp
  hexNum =
    convertHex
      <$> ( satisfy_ (== '0')
              *> satisfy_ (== 'x')
              *> some (satisfy isHexDigit)
          )
      <*> litTyp
  convertDec sign num typ = do
    digits <-
      mapM
        (\c -> if isDigit c then Just (ord c - ord '0') else Nothing)
        num
    let (num', _) =
          foldr
            (\n (num'', pow) -> (n * (10 ^ pow) + num'', num'' + 1))
            (0, 1)
            digits
    case sign of
      Nothing -> return $ Literal (fromIntegral num') typ
      Just '+' -> return $ Literal (fromIntegral num') typ
      Just '-' -> return $ Literal (fromIntegral num' * (-1)) typ
      _ -> Nothing
  convertHex num typ = do
    digits <- mapM (\c -> if isHexDigit c then getHexVal c else Nothing) num
    let (num', _) =
          foldr
            (\n (num'', pow) -> (n * (10 ^ pow) + num'', num'' + 1))
            (0, 1)
            digits
    return $ Literal (fromIntegral num') typ
  getHexVal c
    | isDigit c = Just $ ord c - ord '0'
    | c `elem` ['a' .. 'f'] = Just $ ord c - ord 'a' + 10
    | c `elem` ['A' .. 'F'] = Just $ ord c - ord 'A' + 10
    | otherwise = Nothing
