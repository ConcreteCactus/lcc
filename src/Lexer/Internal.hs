{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Lexer.Internal where

import Control.Applicative
import Control.Monad
import Data.Char
import Data.Functor
import Errors
import Text.Read (readMaybe)

newtype Parser t = Parser
  { runParser :: (TextPos, String) -> Either LexicalError (t, TextPos, String)
  }

instance Functor Parser where
  fmap f (Parser a) =
    Parser
      ( \(pos, str) ->
          case a (pos, str) of
            Left e -> Left e
            Right (b, pos', str') -> Right (f b, pos', str')
      )

instance Monad Parser where
  (Parser a) >>= f =
    Parser
      ( \(pos, str) ->
          case a (pos, str) of
            Left e -> Left e
            Right (b, pos', str') ->
              let Parser g = f b in g (pos', str')
      )

instance Applicative Parser where
  pure a = Parser (\(pos, str) -> Right (a, pos, str))
  f <*> a = do
    x <- f
    x <$> a

instance Alternative Parser where
  empty =
    Parser
      $ const
      $ Left
      $ mkLexErr (0, 0) [LeEndOfStatement]
  (Parser a) <|> (Parser b) = Parser (\s -> a s `or'` b s)
   where
    or' ::
      Either LexicalError a ->
      Either LexicalError a ->
      Either LexicalError a
    or' (Right v) _ = Right v
    or' (Left _) e = e

newtype Ident = Ident String deriving (Eq)

instance Show Ident where
  show (Ident a) = a

unIdent :: Ident -> String
unIdent (Ident s) = s

getPos :: Parser TextPos
getPos = Parser (\(pos, str) -> Right (pos, pos, str))

mapWithError :: (a -> Either LexicalErrorType b) -> Parser a -> Parser b
mapWithError f (Parser pf) =
  Parser
    ( \(pos, str) -> case pf (pos, str) of
      Left e -> Left e
      Right (a, pos', str') -> case f a of
        Left e -> Left $ mkLexErr' pos' e
        Right b -> Right (b, pos', str')
    )

satisfy :: LexicalElement -> (Char -> Bool) -> Parser Char
satisfy e p = Parser parse
 where
  parse (pos, []) = Left $ mkLexErr pos [e]
  parse (pos, c : cs) =
    if p c
      then Right (c, incTextPos pos c, cs)
      else Left $ mkLexErr pos [e]

parseString :: String -> (TextPos, String) -> Maybe (String, TextPos, String)
parseString [] (pos, inp) = Just ([], pos, inp)
parseString _ (_, []) = Nothing
parseString (x : xs) (pos, x' : xs') =
  if x == x'
    then
      ( \(str', pos', rem') ->
          (x : str', pos', rem')
      )
        <$> parseString xs (incTextPos pos x', xs')
    else Nothing

char :: Char -> Parser Char
char c = Parser parseChar
 where
  parseChar (pos, []) = Left $ mkLexErr pos [LeChar c]
  parseChar (pos, x : xs) =
    if x == c
      then Right (x, incTextPos pos x, xs)
      else Left $ mkLexErr pos [LeChar c]

word :: String -> Parser String
word str = Parser (parseString' str)
 where
  -- parseString whatWeWant whatWeHave
  parseString' w (p, h) = case parseString w (p, h) of
    Just a -> Right a
    Nothing -> Left $ mkLexErr p [LeWord str]

operator :: String -> Parser String
operator str = Parser (parseString' str)
 where
  -- parseString whatWeWant whatWeHave
  parseString' w (p, h) = case parseString w (p, h) of
    Just a -> Right a
    Nothing -> Left $ mkLexErr p [LeOperator str]

eof :: Parser ()
eof =
  Parser
    ( \(pos, str) ->
        if null str
          then Right ((), pos, [])
          else Left (mkLexErr pos [LeEndOfStatement])
    )

lexerr :: [LexicalElement] -> Parser a
lexerr ele = Parser (\(pos, _) -> Left $ mkLexErr pos ele)

sepBy1 :: Parser () -> Parser a -> Parser [a]
sepBy1 separator token = (:) <$> token <*> many (separator *> token)

sepBy :: Parser () -> Parser a -> Parser [a]
sepBy separator token = sepBy1 separator token <|> (nothing $> [])

isSimpleSpace :: Char -> Bool
isSimpleSpace '\n' = False
isSimpleSpace s = isSpace s

whiteSpace :: Parser ()
whiteSpace = void $ some $ satisfy LeWhiteSpace isSpace

whiteSpaceO :: Parser ()
whiteSpaceO = whiteSpace <|> nothing

nothing :: Parser ()
nothing = Parser (\(pos, str) -> Right ((), pos, str))

endOfLine :: Parser ()
endOfLine = void (char '\n') <|> void (word "\r\n") <|> lexerr [LeEndOfLine]

endOfLineO :: Parser ()
endOfLineO = endOfLine <|> nothing

lambda :: Parser ()
lambda = void $ operator "\\"

dot :: Parser ()
dot = void $ operator "."

openParen :: Parser ()
openParen = void $ char '('

closeParen :: Parser ()
closeParen = void $ char ')'

isFirstIdentChar :: Char -> Bool
isFirstIdentChar c = isLower c || c == '_'

isIdentChar :: Char -> Bool
isIdentChar c = isAlphaNum c || c == '_'

identifier :: Parser Ident
identifier =
  Ident
    <$> ( (:)
            <$> satisfy LeIdentFirstCharacter isLower
            <*> many (satisfy LeIdentCharacter isIdentChar)
        )

capIdentifier :: Parser String
capIdentifier =
  (:)
    <$> satisfy LeUppercaseCharacter isUpper
    <*> many (satisfy LeUppercaseCharacter isAlphaNum)

anyCapIdentifier :: Parser String
anyCapIdentifier =
  (:)
    <$> satisfy LeAlphabeticalCharacter isAlpha
    <*> many (satisfy LeAlphanumericCharacter isAlphaNum)

arrow :: Parser ()
arrow = void $ operator "->"

colon :: Parser ()
colon = void $ operator ":"

colonEquals :: Parser ()
colonEquals = void $ operator ":="

collapseMaybe :: Parser (Maybe a) -> Parser a
collapseMaybe (Parser fMa) =
  Parser
    ( \s1 -> case fMa s1 of
        Left err -> Left err
        Right (Nothing, pos, _) -> Left $ mkLexErr pos [LeNumber]
        Right (Just a, pos, s2) -> Right (a, pos, s2)
    )

b10Integer :: Parser Integer
b10Integer = collapseMaybe $ readMaybe <$> some (satisfy LeNumber isDigit)

readb16Integer :: String -> Integer
readb16Integer s =
  foldr
    (\(c, n) sm -> toDigit c * (16 ^ n) + sm)
    0
    (zip s [(length s - 1), (length s - 2) ..])
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
            ( word "0x" <|> word "0X"
            )
            *> some
              ( satisfy
                  LeHexDigit
                  ( \a -> isDigit a || a `elem` "abcdefABCDEF"
                  )
              )
        )
    <* word "_"

integer :: Parser Integer
integer = b16Integer <|> b10Integer

whiteSpaceS :: Parser String
whiteSpaceS = some $ satisfy LeWhiteSpace isSimpleSpace

whiteSpaceSO :: Parser String
whiteSpaceSO = many $ satisfy LeWhiteSpace isSimpleSpace

stripLine :: String -> String
stripLine line = reverse $ dropWhile isSpace $ reverse starting
 where
  starting :: String
  starting = dropWhile isSpace line

emptyLine :: Parser ()
emptyLine = void $ whiteSpaceSO *> char '\n'

statement :: Parser (TextPos, String)
statement = do
  _ <-
    many
      $ many (satisfy LeWhiteSpace isSimpleSpace)
      <* satisfy LeEndOfLine (== '\n')
  startPos <- getPos
  sc <- satisfy LeNotWhiteSpace (not . isSpace)
  lc <- many $ satisfy LeNotNewLine (/= '\n')
  llc <-
    many
      ( do
          _ <- many $ satisfy LeEndOfLine (== '\n')
          sp <- some $ satisfy LeWhiteSpace isSimpleSpace
          lc' <- many $ satisfy LeNotNewLine (/= '\n')
          return $ sp ++ lc'
      )
  return (startPos, sc : lc ++ concat llc)

-- -- -- Unit tests
-- --
-- -- tests :: [Bool]
-- -- tests =
-- --   [ runParser whiteSpace " " == Right ((), ""),
-- --     runParser whiteSpace "  " == Right ((), ""),
-- --     runParser whiteSpace "\t \ta" == Right ((), "a"),
-- --     runParser whiteSpace "  a" == Right ((), "a"),
-- --     runParser whiteSpace "  \t\n hello" == Right ((), "hello"),
-- --     runParser whiteSpace " \t \n" == Right ((), ""),
-- --     runParser whiteSpace " \t \nhello" == Right ((), "hello"),
-- --     runParser b10Integer "34249840" == Right (34249840, ""),
-- --     runParser b16Integer "0x20A9C70" == Right (34249840, ""),
-- --     runParser block "a : b -> c \na := \\b.c" == Right ("a : b -> c", "\na := \\b.c"),
-- --     runParser block "a := \\b.\n c" == Right ("a := \\b. c", ""),
-- --     runParser block "a : b -> c \na := \\b.c" == Right ("a : b -> c", "\na := \\b.c"),
-- --     runParser block "a : b ->\n c \r\na := \\b.c" == Right ("a : b -> c", "\na := \\b.c"),
-- --     runParser block "a : b -> c \r\na := \\b.c" == Right ("a : b -> c", "\na := \\b.c"),
-- --     runParser block "a\n :\n b\n ->\n c \r\na := \\b.c" == Right ("a : b -> c", "\na := \\b.c"),
-- --     runParser identifier "a" == Right (Ident "a", ""),
-- --     runParser identifier "a1" == Right (Ident "a1", ""),
-- --     runParser identifier "A1b" == Left (LexicalError CaseError),
-- --     runParser capIdentifier "A" == Right ("A", ""),
-- --     runParser capIdentifier "A1" == Right ("A1", ""),
-- --     runParser capIdentifier "a1" == Left (LexicalError CaseError)
-- --   ]
