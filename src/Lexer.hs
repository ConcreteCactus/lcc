{-# OPTIONS_GHC -Wincomplete-patterns #-}

module Lexer
  ( CompilerError (..),
    Parser (..),
    Ident (..),
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
    integer,
    block,
    eof,
    capIdentifier,
    anyCapIdentifier,
    (<|>),
    unIdent,
  )
where

import Control.Applicative
import Control.Monad
import Data.Bifunctor
import Data.Char
import Data.Functor
import Errors
import Text.Read (readMaybe)

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

newtype Ident = Ident String deriving (Show, Eq)

unIdent :: Ident -> String
unIdent (Ident s) = s

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
sepBy separator token = sepBy1 separator token <|> (nothing $> [])

isSimpleSpace :: Char -> Bool
isSimpleSpace '\n' = False
isSimpleSpace s = isSpace s

whiteSpace :: Parser ()
whiteSpace = void $ some $ satisfyE CompilerError isSpace

whiteSpaceO :: Parser ()
whiteSpaceO = whiteSpace <|> nothing

nothing :: Parser ()
nothing = Parser (\s -> Right ((), s))

eof :: Parser ()
eof = Parser (\s -> if s == "" then Right ((), "") else Left (LexicalError EndOfFileExpected))

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

identifier :: Parser Ident
identifier = Ident <$> ((:) <$> satisfyE (LexicalError CaseError) isLower <*> many (satisfyE CompilerError isAlphaNum))

capIdentifier :: Parser String
capIdentifier =
  (:)
    <$> satisfyE (LexicalError CaseError) isUpper
    <*> many (satisfyE CompilerError isAlphaNum)

anyCapIdentifier :: Parser String
anyCapIdentifier =
  (:)
    <$> satisfyE (LexicalError OtherError) isAlpha
    <*> many (satisfyE CompilerError isAlphaNum)

arrow :: Parser ()
arrow = void $ stringE CompilerError "->"

colon :: Parser ()
colon = void $ charE CompilerError ':'

colonEquals :: Parser ()
colonEquals = void $ stringE CompilerError ":="

collapseMaybe :: Parser (Maybe a) -> Parser a
collapseMaybe (Parser fMa) =
  Parser
    ( \s1 -> case fMa s1 of
        Left err -> Left err
        Right (Nothing, _) -> Left CompilerError
        Right (Just a, s2) -> Right (a, s2)
    )

b10Integer :: Parser Integer
b10Integer = collapseMaybe $ readMaybe <$> some (satisfyE CompilerError isDigit)

readb16Integer :: String -> Integer
readb16Integer s = foldr (\(c, n) sm -> toDigit c * (16 ^ n) + sm) 0 (zip s [(length s - 1), (length s - 2) ..])
  where
    toDigit :: Char -> Integer
    toDigit c
      | c0 <= cc && cc <= c9 = cc - c0
      | ca <= cc && cc <= cf = cc - ca + 10
      | otherwise = cc - cA + 10
      where
        cc :: Integer
        cc = fromIntegral $ ord c
        c0 :: Integer
        c0 = fromIntegral $ ord '0'
        c9 :: Integer
        c9 = fromIntegral $ ord '9'
        ca :: Integer
        ca = fromIntegral $ ord 'a'
        cf :: Integer
        cf = fromIntegral $ ord 'f'
        cA :: Integer
        cA = fromIntegral $ ord 'A'

b16Integer :: Parser Integer
b16Integer =
  readb16Integer
    <$> ( void
            ( stringE CompilerError "0x" <|> stringE CompilerError "0X"
            )
            *> some
              ( satisfyE
                  CompilerError
                  ( \a -> isDigit a || a `elem` "abcdefABCDEF"
                  )
              )
        )

integer :: Parser Integer
integer = b16Integer <|> b10Integer

whiteSpaceS :: Parser String
whiteSpaceS = some $ satisfyE CompilerError isSimpleSpace

whiteSpaceSO :: Parser String
whiteSpaceSO = many $ satisfyE CompilerError isSimpleSpace

stripLine :: String -> String
stripLine line = reverse $ dropWhile isSpace $ reverse starting
  where
    starting :: String
    starting = dropWhile isSpace line

emptyLine :: Parser ()
emptyLine = void $ whiteSpaceSO *> charE CompilerError '\n'

replaceParsed :: Parser a -> String -> String -> String
replaceParsed parser replacement [] = case runParser parser [] of
  Left _ -> []
  Right _ -> replacement
replaceParsed parser replacement (r : replacee) = case runParser parser (r : replacee) of
  Left _ -> r : replaceParsed parser replacement replacee
  Right (_, remaining) -> replacement ++ replaceParsed parser replacement remaining

block :: Parser String
block = do
  _ <- many emptyLine
  indent <- whiteSpaceSO
  line <- some $ satisfyE CompilerError (/= '\n')
  remLines <-
    sepBy
      ( void $
          ( some emptyLine *> stringE CompilerError indent
          )
            *> whiteSpaceS
      )
      $ many
      $ satisfyE CompilerError (/= '\n')
  return $
    unwords $
      map (replaceParsed whiteSpace " ") $
        filter (not . null) $
          map stripLine $
            line : remLines

-- Unit tests

tests :: [Bool]
tests =
  [ runParser whiteSpace " " == Right ((), ""),
    runParser whiteSpace "  " == Right ((), ""),
    runParser whiteSpace "\t \ta" == Right ((), "a"),
    runParser whiteSpace "  a" == Right ((), "a"),
    runParser whiteSpace "  \t\n hello" == Right ((), "hello"),
    runParser whiteSpace " \t \n" == Right ((), ""),
    runParser whiteSpace " \t \nhello" == Right ((), "hello"),
    runParser b10Integer "34249840" == Right (34249840, ""),
    runParser b16Integer "0x20A9C70" == Right (34249840, ""),
    runParser block "a : b -> c \na := \\b.c" == Right ("a : b -> c", "\na := \\b.c"),
    runParser block "a := \\b.\n c" == Right ("a := \\b. c", ""),
    runParser block "a : b -> c \na := \\b.c" == Right ("a : b -> c", "\na := \\b.c"),
    runParser block "a : b ->\n c \r\na := \\b.c" == Right ("a : b -> c", "\na := \\b.c"),
    runParser block "a : b -> c \r\na := \\b.c" == Right ("a : b -> c", "\na := \\b.c"),
    runParser block "a\n :\n b\n ->\n c \r\na := \\b.c" == Right ("a : b -> c", "\na := \\b.c"),
    runParser identifier "a" == Right (Ident "a", ""),
    runParser identifier "a1" == Right (Ident "a1", ""),
    runParser identifier "A1b" == Left (LexicalError CaseError),
    runParser capIdentifier "A" == Right ("A", ""),
    runParser capIdentifier "A1" == Right ("A1", ""),
    runParser capIdentifier "a1" == Left (LexicalError CaseError)
  ]
