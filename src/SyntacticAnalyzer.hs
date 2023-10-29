{-# OPTIONS_GHC -Wincomplete-patterns #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

module SyntacticAnalyzer
  ( Expression (..),
    Type (..),
    ProgramPart (..),
    Literal (..),
    Program,
    parseExpression,
    parseType,
    parseProgram,
    parseProgramSingleError,
    syntacticAnalyzerTests,
  )
where

import Control.Applicative
import qualified Data.List.NonEmpty as Ne
import Errors
import Lexer

newtype Literal = IntegerLiteral Integer deriving (Eq)

instance Show Literal where
  show (IntegerLiteral n) = show n

data Expression
  = Id Ident
  | Lit Literal
  | Lambda Ident Expression
  | Application Expression Expression
  deriving (Show, Eq)

data AtomicType = AInt deriving (Show, Eq)

data Type
  = TypeId Ident
  | TypeName AtomicType
  | FunctionType Type Type
  deriving (Show, Eq)

data ProgramPart = Definition Ident Expression | Declaration Ident Type deriving (Eq, Show)

type Program = [ProgramPart]

idParser :: Parser Expression
idParser = Id <$> identifier

integerLiteralParser :: Parser Literal
integerLiteralParser = IntegerLiteral <$> integer

literalParser :: Parser Expression
literalParser = Lit <$> integerLiteralParser

lambdaParser :: Parser Expression
lambdaParser =
  Lambda
    <$> ( lambda
            *> (whiteSpaceO *> identifier <* whiteSpaceO)
            <* (dot <* whiteSpaceO)
        )
    <*> expressionParser

applicationsParser :: Parser Expression
applicationsParser = foldl1 Application <$> sepBy1 whiteSpace parseExpressionWithoutApplication

parseExpressionWithoutApplication :: Parser Expression
parseExpressionWithoutApplication =
  literalParser
    <|> idParser
    <|> lambdaParser
    <|> (openParen *> expressionParser <* closeParen)

expressionParser :: Parser Expression
expressionParser =
  applicationsParser
    <|> literalParser
    <|> idParser
    <|> lambdaParser
    <|> ( whiteSpaceO
            *> ( openParen
                   *> (whiteSpaceO *> expressionParser <* whiteSpaceO)
                   <* closeParen
               )
        )

parseExpression :: String -> Either CompilerError Expression
parseExpression s = fst <$> runParser expressionParser s

typeIdParser :: Parser Type
typeIdParser = TypeId <$> identifier

typeNameParser :: Parser Type
typeNameParser = TypeName AInt <$ capIdentifier

functionParser :: Parser Type
functionParser = FunctionType <$> typeParserWithoutFunction <*> (whiteSpaceO *> arrow *> whiteSpaceO *> typeParser)

typeParserWithoutFunction :: Parser Type
typeParserWithoutFunction =
  typeIdParser
    <|> typeNameParser
    <|> ( whiteSpaceO
            *> ( openParen
                   *> (whiteSpaceO *> typeParser <* whiteSpaceO)
                   <* closeParen
               )
            <* whiteSpaceO
        )

typeParser :: Parser Type
typeParser =
  functionParser
    <|> typeIdParser
    <|> typeNameParser
    <|> ( whiteSpaceO
            *> ( openParen
                   *> (whiteSpaceO *> typeParser <* whiteSpaceO)
                   <* closeParen
               )
            <* whiteSpaceO
        )

parseType :: String -> Either CompilerError Type
parseType s = fst <$> runParser typeParser s

declarationParser :: Parser ProgramPart
declarationParser = Declaration <$> identifier <*> (whiteSpaceO *> colon *> whiteSpaceO *> typeParser)

definitionParser :: Parser ProgramPart
definitionParser = Definition <$> identifier <*> (whiteSpaceO *> colonEquals *> whiteSpaceO *> expressionParser)

blocksParser :: Parser [String]
blocksParser = filter (not . null) <$> sepBy endOfLine block <* whiteSpaceO <* eof

partParser :: Parser ProgramPart
partParser = definitionParser <|> declarationParser

parseProgram :: String -> Either (Ne.NonEmpty CompilerError) Program
parseProgram s = do
  blocks <- arrayifyError blocksE
  let parsedBlocks = fmap (fmap fst . runParser partParser) blocks
  foldr
    ( \b acc -> case (b, acc) of
        (Left e, Left eacc) -> Left $ e `Ne.cons` eacc
        (Left e, _) -> Left $ Ne.singleton e
        (Right a, Right aacc) -> Right $ a : aacc
        (Right _, e) -> e
    )
    (Right [])
    parsedBlocks
  where
    blocksE :: Either CompilerError [String]
    blocksE = fst <$> runParser blocksParser s
    arrayifyError :: Either e a -> Either (Ne.NonEmpty e) a
    arrayifyError (Left e) = Left $ Ne.singleton e
    arrayifyError (Right a) = Right a

parseProgramSingleError :: String -> Either CompilerError Program
parseProgramSingleError s = case parseProgram s of
  Left e -> Left $ Ne.head e
  Right a -> Right a

-- programParser :: Parser Program
-- programParser =
--   Parser
--     ( \s -> case parseProgram s of
--         Left e -> Left $ Ne.head e
--         Right a -> Right (a, "")
--     )

-- Unit tests

idI :: String -> Expression
idI = Id . Ident

lambdaI :: String -> Expression -> Expression
lambdaI = Lambda . Ident

declI :: String -> Type -> ProgramPart
declI = Declaration . Ident

defI :: String -> Expression -> ProgramPart
defI = Definition . Ident

typeIdI :: String -> Type
typeIdI = TypeId . Ident

tint :: Type
tint = TypeName AInt

syntacticAnalyzerTests :: [Bool]
syntacticAnalyzerTests =
  -- Id tests
  [ runParser expressionParser "hello" == Right (idI "hello", ""),
    runParser expressionParser "hello   " == Right (idI "hello", "   "),
    runParser expressionParser "   " == Left (LexicalError UnexpectedEndOfFile),
    -- Literal tests
    runParser expressionParser "154381" == Right (Lit (IntegerLiteral 154381), ""),
    runParser expressionParser "15 4381" == Right (Application (Lit (IntegerLiteral 15)) (Lit (IntegerLiteral 4381)), ""),
    runParser expressionParser "0xdeadbeef" == Right (Lit (IntegerLiteral 3735928559), ""),
    -- Lambda tests
    runParser expressionParser "\\a.b" == Right (lambdaI "a" (idI "b"), ""),
    runParser expressionParser "\\a. b" == Right (lambdaI "a" (idI "b"), ""),
    runParser expressionParser "\\a . b" == Right (lambdaI "a" (idI "b"), ""),
    runParser expressionParser "\\ a . b" == Right (lambdaI "a" (idI "b"), ""),
    runParser expressionParser "\\ a.b" == Right (lambdaI "a" (idI "b"), ""),
    runParser expressionParser "\\a .b" == Right (lambdaI "a" (idI "b"), ""),
    runParser expressionParser "\\a.b  " == Right (lambdaI "a" (idI "b"), "  "),
    runParser expressionParser "\\a.b  b" == Right (lambdaI "a" (Application (idI "b") (idI "b")), ""),
    -- Application tests
    runParser expressionParser "a  b" == Right (Application (idI "a") (idI "b"), ""),
    runParser expressionParser "(\\a.b)  b" == Right (Application (lambdaI "a" (idI "b")) (idI "b"), ""),
    runParser expressionParser "a b" == Right (Application (idI "a") (idI "b"), ""),
    runParser expressionParser "a b c" == Right (Application (Application (idI "a") (idI "b")) (idI "c"), ""),
    runParser expressionParser "a b c d" == Right (Application (Application (Application (idI "a") (idI "b")) (idI "c")) (idI "d"), ""),
    runParser expressionParser "(\\a.a) b c" == Right (Application (Application (lambdaI "a" (idI "a")) (idI "b")) (idI "c"), ""),
    runParser expressionParser "(\\a.\\b.b) b c" == Right (Application (Application (lambdaI "a" (lambdaI "b" (idI "b"))) (idI "b")) (idI "c"), ""),
    -- Type parser tests
    runParser typeParser "a" == Right (typeIdI "a", ""),
    runParser typeParser "ab" == Right (typeIdI "ab", ""),
    runParser typeParser "ab1  " == Right (typeIdI "ab1", "  "),
    runParser typeParser "a -> b" == Right (FunctionType (typeIdI "a") (typeIdI "b"), ""),
    runParser typeParser "a->b" == Right (FunctionType (typeIdI "a") (typeIdI "b"), ""),
    runParser typeParser "a -> b -> c" == Right (FunctionType (typeIdI "a") (FunctionType (typeIdI "b") (typeIdI "c")), ""),
    runParser typeParser "(a -> b) -> c" == Right (FunctionType (FunctionType (typeIdI "a") (typeIdI "b")) (typeIdI "c"), ""),
    runParser typeParser "Int -> Int -> Int" == Right (FunctionType (FunctionType tint tint) tint, ""),
    -- Declaration definition
    runParser declarationParser "hello : string" == Right (declI "hello" (typeIdI "string"), ""),
    runParser declarationParser "hello : String" == Right (declI "hello" (typeIdI "String"), ""),
    runParser declarationParser "helloWorld : string -> string" == Right (declI "helloWorld" (FunctionType (typeIdI "string") (typeIdI "string")), ""),
    runParser definitionParser "world := \\a.\\b.a" == Right (defI "world" (lambdaI "a" (lambdaI "b" (idI "a"))), ""),
    -- Whole program parsing
    parseProgram program1 == Right program1ShouldBe,
    parseProgram program2 == Right program2ShouldBe,
    parseProgram program3 == Right program3ShouldBe
  ]

program1 :: String
program1 =
  "hello : string\n"
    ++ "hello := helloString"

program1ShouldBe :: Program
program1ShouldBe =
  [ declI "hello" (typeIdI "string"),
    defI "hello" (idI "helloString")
  ]

program2 :: String
program2 =
  "hello : string -> (string -> int) -> char\t   \n \n \n   \n\n\r\n\n  \t \n"
    ++ "hello := \n \t \\a.\\b.\\c.b  \n"

program2ShouldBe :: Program
program2ShouldBe =
  [ declI "hello" (FunctionType (typeIdI "string") (FunctionType (FunctionType (typeIdI "string") (typeIdI "int")) (typeIdI "char"))),
    defI "hello" (lambdaI "a" (lambdaI "b" (lambdaI "c" (idI "b"))))
  ]

program3 :: String
program3 =
  "true : a -> a -> a\n"
    ++ "true := \\a.\\b.a\n"
    ++ "false : a -> a -> a\n"
    ++ "false := \\a.\\b.b\n"
    ++ "one : number\n"
    ++ "one := 1"

program3ShouldBe :: Program
program3ShouldBe =
  [ declI "true" (FunctionType (typeIdI "a") (FunctionType (typeIdI "a") (typeIdI "a"))),
    defI "true" (lambdaI "a" (lambdaI "b" (idI "a"))),
    declI "false" (FunctionType (typeIdI "a") (FunctionType (typeIdI "a") (typeIdI "a"))),
    defI "false" (lambdaI "a" (lambdaI "b" (idI "b"))),
    declI "one" (typeIdI "number"),
    defI "one" (Lit (IntegerLiteral 1))
  ]
