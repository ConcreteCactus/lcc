{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Lexer.Internal where

import AtomicType
import Control.Applicative
import Control.Monad
import Data.Char
import Util

-- import Debug.Trace
import Errors

type SourceCode = String

newtype VarIdent = VarIdent String deriving (Eq)
newtype TypIdent = TypIdent String deriving (Eq)
newtype TypName = TypName String deriving (Eq)
data Literal = Literal String AtomicType deriving (Eq)

newtype Parser e a = Parser
    { runParser ::
        (TextPos, SourceCode) ->
        Either e (TextPos, a, SourceCode)
    }

type ParserP = Parser TextPos
type ParserE = Parser LexicalError

instance Show VarIdent where
    show (VarIdent i) = i
instance Show TypIdent where
    show (TypIdent i) = i
instance Show Literal where
    show (Literal n typ) = show n ++ typeNameOf typ

instance Functor (Parser e) where
    fmap f (Parser fa) =
        Parser
            ( \(textPos, sourceCode) -> do
                (textPos', parsedValue, sourceCode') <- fa (textPos, sourceCode)
                return (textPos', f parsedValue, sourceCode')
            )

instance Applicative (Parser e) where
    pure a = Parser (\(textPos, sourceCode) -> Right (textPos, a, sourceCode))
    (Parser ff) <*> (Parser fa) =
        Parser
            ( \(textPos, sourceCode) -> do
                (textPos', f, sourceCode') <- ff (textPos, sourceCode)
                (textPos'', parsedValue, sourceCode'') <- fa (textPos', sourceCode')
                return (textPos'', f parsedValue, sourceCode'')
            )

instance (Default e) => Alternative (Parser e) where
    (Parser fa) <|> (Parser fb) =
        Parser
            ( \(textPos, sourceCode) -> case fa (textPos, sourceCode) of
                Right ok -> Right ok
                Left _ -> fb (textPos, sourceCode)
            )
    empty = Parser $ const $ Left def

instance Monad (Parser e) where
    (Parser fa) >>= fp =
        Parser
            ( \(textPos, sourceCode) -> do
                (textPos', parsedValue, sourceCode') <- fa (textPos, sourceCode)
                let (Parser fa') = fp parsedValue
                fa' (textPos', sourceCode')
            )

isMain :: VarIdent -> Bool
isMain (VarIdent "main") = True
isMain _ = False

collapseMaybe :: ParserP (Maybe a) -> ParserP a
collapseMaybe (Parser fa) =
    Parser
        ( \a -> case fa a of
            Right (textPos, Just parsedValue, sourceCode) ->
                Right (textPos, parsedValue, sourceCode)
            Right (textPos, Nothing, _) -> Left textPos
            Left textPos -> Left textPos
        )

collapseEither :: ParserE (Either LexicalErrorType a) -> ParserE a
collapseEither (Parser fa) =
    Parser
        ( \a -> case fa a of
            Right (textPos, Right parsedValue, sourceCode) ->
                Right (textPos, parsedValue, sourceCode)
            Right (textPos, Left e, _) -> Left (mkLexErr' textPos e)
            Left e -> Left e
        )

satisfy :: (Char -> Bool) -> ParserP Char
satisfy f =
    Parser
        ( \(textPos, sourceCode) -> case sourceCode of
            (c : cs) | f c -> Right (incTextPos c textPos, c, cs)
            _otherwise -> Left textPos
        )

satisfy_ :: (Char -> Bool) -> ParserP ()
satisfy_ f = void $ satisfy f

withError :: ParserP a -> LexicalErrorType -> ParserE a
withError (Parser fa) e =
    Parser
        ( \(textPos, sourceCode) ->
            case fa (textPos, sourceCode) of
                Right (textPos', parsedValue, sourceCode') ->
                    Right (textPos', parsedValue, sourceCode')
                Left textPos' -> Left $ mkLexErr' textPos' e
        )

infixl 4 `withError`

withExpected :: ParserP a -> LexicalElement -> ParserE a
withExpected p l = p `withError` LeUnexpectedLexicalElement [l]

infixl 4 `withExpected`

sepBy1 :: ParserE () -> ParserE a -> ParserE [a]
sepBy1 delim pars = (:) <$> pars <*> many (delim *> pars)

sepBy2 :: ParserE () -> ParserE a -> ParserE [a]
sepBy2 delim pars = (:) <$> pars <*> some (delim *> pars)

sepBy :: ParserE () -> ParserE a -> ParserE [a]
sepBy delim pars = ([] <$ endOfFile) <|> sepBy1 delim pars

leftAssoc :: (a -> a -> a) -> ParserE () -> ParserE a -> ParserE a
leftAssoc f delim pars = foldl1 f <$> sepBy1 delim pars

leftAssoc2 :: (a -> a -> a) -> ParserE () -> ParserE a -> ParserE a
leftAssoc2 f delim pars = foldl1 f <$> sepBy2 delim pars

colon :: ParserE ()
colon = satisfy_ (== ':') `withExpected` LeColon

arrow :: ParserE ()
arrow = satisfy_ (== '-') *> satisfy_ (== '>') `withExpected` LeColon

plus :: ParserE ()
plus = satisfy_ (== '+') `withExpected` LeColon

star :: ParserE ()
star = satisfy_ (== '*') `withExpected` LeColon

colonEquals :: ParserE ()
colonEquals =
    satisfy_ (== ':')
        *> satisfy_ (== '=')
            `withExpected` LeColonEquals

backSlash :: ParserE ()
backSlash = satisfy_ (== '\\') `withExpected` LeBackslash

dot :: ParserE ()
dot = satisfy_ (== '.') `withExpected` LeBackslash

exprWhiteSpace' :: ParserP ()
exprWhiteSpace' = void $ some (space <|> newln)
  where
    space = satisfy_ (`elem` " \t")
    newln = some (optional (satisfy_ (== '\r')) *> satisfy_ (== '\n')) *> space

exprWhiteSpace :: ParserE ()
exprWhiteSpace = exprWhiteSpace' `withExpected` LeExprWhiteSpace

stmtWhiteSpace :: ParserE ()
stmtWhiteSpace =
    void $
        optional exprWhiteSpace'
            *> some (optional (satisfy_ (== '\r')) *> satisfy_ (== '\n'))
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
        <$> filterKeywords
            ["if", "then", "else"]
            ( (:)
                <$> satisfy isLower
                <*> many (satisfy (\c -> isAlphaNum c || c == '_'))
            )
            `withExpected` LeVarIdent

filterKeywords :: [String] -> ParserP String -> ParserP String
filterKeywords ks p =
    collapseMaybe $
        (\w -> if w `elem` ks then Nothing else Just w)
            <$> p

typIdent :: ParserE TypIdent
typIdent =
    TypIdent
        <$> ( (:)
                <$> satisfy (\c -> isAlpha c && isLower c)
                <*> many (satisfy isAlphaNum)
            )
            `withExpected` LeTypIdent

typName :: ParserE TypName
typName =
    TypName
        <$> ( (:)
                <$> satisfy (\c -> isAlpha c && isUpper c)
                <*> many (satisfy isAlphaNum)
            )
            `withExpected` LeTypIdent

nothingParser :: ParserP a
nothingParser = Parser (\(textPos, _) -> Left textPos)

litPostfix :: [AtomicType] -> ParserP AtomicType
litPostfix =
    foldr
        ( \typ pars ->
            ( mapM (satisfy . (==)) (typeNameOf typ) *> pure typ
            )
                <|> pars
        )
        nothingParser

literal :: ParserE Literal
literal =
    (charLit <|> hexNum <|> floatNum <|> decNum)
        `withExpected` LeLiteral
  where
    decNum =
        (\sign num typ -> Literal (maybe num (: num) sign) typ)
            <$> optional (satisfy (`elem` "+-"))
            <*> some (satisfy isDigit)
            <*> litPostfix (signedIntAtomicTypes ++ unsignedIntAtomicTypes)
    hexNum =
        (\num typ -> Literal ("0x" ++ num) typ)
            <$> ( satisfy_ (== '0')
                    *> (satisfy_ (== 'x') <|> satisfy_ (== 'X'))
                    *> some (satisfy isHexDigit)
                    <* satisfy (== '_')
                )
            <*> litPostfix unsignedIntAtomicTypes
    charLit =
        charToLiteral
            <$> (satisfy_ (== '\'') *> satisfy isPrint)
            <* satisfy_ (== '\'')
    charToLiteral :: Char -> Literal
    charToLiteral c = Literal ['\'', c, '\''] AChar
    floatNum =
        (\w f t -> Literal (maybe w ((w ++ ".") ++) f) t)
            <$> some (satisfy isDigit)
            <*> optional (satisfy_ (== '.') *> some (satisfy isDigit))
            <*> litPostfix floatAtomicTypes

openingBracket :: ParserE ()
openingBracket = satisfy_ (== '(') `withExpected` LeOpeningBracket

closingBracket :: ParserE ()
closingBracket = satisfy_ (== ')') `withExpected` LeClosingBracket

openingSquareBracket :: ParserE ()
openingSquareBracket = satisfy_ (== '[') `withExpected` LeOpeningSquareBracket

closingSquareBracket :: ParserE ()
closingSquareBracket = satisfy_ (== ']') `withExpected` LeClosingSquareBracket

endOfFile :: ParserE ()
endOfFile =
    optional stmtWhiteSpace
        *> optional exprWhiteSpace
        *> (endOfFile' `withExpected` LeEndOfFile)

endOfFile' :: ParserP ()
endOfFile' =
    Parser
        ( \(textPos, sourceCode) ->
            if null sourceCode
                then Right (textPos, (), sourceCode)
                else Left textPos
        )

currentPos :: ParserE TextPos
currentPos =
    Parser
        ( \(textPos, sourceCode) ->
            Right (textPos, textPos, sourceCode)
        )

execParser :: ParserE a -> SourceCode -> Either LexicalError a
execParser (Parser fa) sc = (\(_, val, _) -> val) <$> fa ((1, 1), sc)

-- traceParser :: String -> Parser ()
-- traceParser str =
--     Parser
--         ( \(textPos, sourceCode) -> trace (str ++ " " ++ show textPos) $ Right (textPos, (), sourceCode)
--         )
