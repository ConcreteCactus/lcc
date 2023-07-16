{-# OPTIONS_GHC -Wincomplete-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use tuple-section" #-}
{-# HLINT ignore "Use execState" #-}

module SemanticAnalyzer
  ( SemanticError (..),
    Identifier (..),
    SemExpression (..),
    SemProgram (..),
    SemProgramPart (..),
    Env (..),
    State (..),
    startingEnv,
    createSemanticExpression,
    createSemanticExpressionS,
    createSemanticProgram,
    createSemanticProgramS,
    parseAndCreateProgram,
    parseAndCreateExpressionWithProgram,
    emptyProgram,
    semanticAnalyzerTests,
  )
where

import Data.Foldable
import Errors
import SyntacticAnalyzer
import Util

data Identifier = Identifier Int String deriving (Eq)

data SemTypeIdentifier = TInteger deriving (Show, Eq)

data InferredIdentifier = TIInteger deriving (Show, Eq)

data SemExpression
  = SId Identifier
  | SRef String
  | SLit Literal
  | SLambda String SemExpression
  | SApplication SemExpression SemExpression
  deriving (Eq)

data SemType
  = STypeId SemTypeIdentifier
  | SFunctionType SemType SemType
  deriving (Eq, Show)

data InferredType
  = ISTypeId InferredIdentifier
  | ISFunctionType InferredType InferredType
  deriving (Eq, Show)

data Env = Env {globals :: [String], scope :: [String]} deriving (Eq, Show)

data SemProgramPart = SemDeclaration | SemDefinition String SemExpression deriving (Eq)

data SemProgram = SemProgram [String] [SemProgramPart] deriving (Eq, Show)

instance Show SemProgramPart where
  show SemDeclaration = "Declaration"
  show (SemDefinition name expr) = name ++ " := " ++ show expr

instance Show SemExpression where
  show (SId identifier) = show identifier
  show (SRef name) = name
  show (SLit literal) = show literal
  show (SLambda paramName expr) = "λ" ++ paramName ++ "." ++ show expr
  show (SApplication expr1@(SLambda _ _) expr2@(SApplication _ _)) =
    "(" ++ show expr1 ++ ") (" ++ show expr2 ++ ")"
  show (SApplication expr1@(SLambda _ _) expr2) =
    "(" ++ show expr1 ++ ") " ++ show expr2
  show (SApplication expr1 expr2@(SApplication _ _)) =
    show expr1 ++ " (" ++ show expr2 ++ ")"
  show (SApplication expr1 expr2) = show expr1 ++ " " ++ show expr2

instance Show Identifier where
  show (Identifier code name) = name ++ "_" ++ show code

startingEnv :: Env
startingEnv = Env [] []

addGlobal :: String -> State Env ()
addGlobal newGlobal = do
  env <- get
  put $ env {globals = newGlobal : globals env}

addVar :: String -> State Env ()
addVar newVar = do
  env <- get
  put $ env {scope = newVar : scope env}

popVar :: State Env ()
popVar = do
  env <- get
  put $ env {scope = tail $ scope env}

findVar :: String -> State Env (Maybe Identifier)
findVar idName = do
  env <- get
  let scopeVars = scope env
  let (foundNameM, count) = foldr (\name (found, n) -> if name == idName then (Just name, 1) else (found, n + 1)) (Nothing, 0) scopeVars
  case foundNameM of
    Nothing -> return Nothing
    Just foundName -> return $ Just $ Identifier count foundName

findGlobal :: String -> State Env (Maybe String)
findGlobal idName = do
  env <- get
  let globalVars = globals env
  let foundNameM = find (== idName) globalVars
  return foundNameM

createSemanticType :: SynTypeExpression -> Either CompilerError SemType
createSemanticType (TypeId "int") = Right $ STypeId TInteger
createSemanticType (FunctionType t1 t2) =
  SFunctionType
    <$> createSemanticType t1
    <*> createSemanticType t2
createSemanticType _ = Left CompilerError

createSemanticExpressionS ::
  SynExpression -> State Env (Either CompilerError SemExpression)
createSemanticExpressionS (Id name) = do
  identM <- findVar name
  case identM of
    Just ident -> return $ Right $ SId ident
    Nothing -> do
      globalM <- findGlobal name
      case globalM of
        Just global -> return $ Right $ SRef global
        Nothing -> return $ Left $ SemanticError $ SUndefinedVariable name
createSemanticExpressionS (Lambda param expr) = do
  addVar param
  sformula <- createSemanticExpressionS expr
  popVar
  return $ SLambda param <$> sformula
createSemanticExpressionS (Application func arg) = do
  sfunc <- createSemanticExpressionS func
  sarg <- createSemanticExpressionS arg
  return $ SApplication <$> sfunc <*> sarg
createSemanticExpressionS (Lit l) = return $ Right $ SLit l

createSemanticExpression :: SynExpression -> Either CompilerError SemExpression
createSemanticExpression expr = snd $ runState (createSemanticExpressionS expr) startingEnv

createSemanticProgramS :: Program -> State Env (Either CompilerError [SemProgramPart])
createSemanticProgramS [] = return $ Right []
createSemanticProgramS (SynDeclaration _ _ : rest) = createSemanticProgramS rest
createSemanticProgramS (SynDefinition name synExpr : rest) = do
  nameIdentM <- findGlobal name
  case nameIdentM of
    Just _ -> return $ Left (SemanticError ValueRedefinition)
    Nothing -> do
      addGlobal name
      expr <- createSemanticExpressionS synExpr
      semRest <- createSemanticProgramS rest
      return $ (\e r -> SemDefinition name e : r) <$> expr <*> semRest

createSemanticProgram :: Program -> Either CompilerError SemProgram
createSemanticProgram prog = SemProgram globs <$> semProgParts
  where
    (Env globs _, semProgParts) = runState (createSemanticProgramS prog) startingEnv

parseAndCreateProgram :: String -> Either CompilerError SemProgram
parseAndCreateProgram s = parseProgramSingleError s >>= createSemanticProgram

parseAndCreateExpressionWithProgram :: SemProgram -> String -> Either CompilerError SemExpression
parseAndCreateExpressionWithProgram (SemProgram globs _) s = do
  parsed <- parseExpression s
  execState (createSemanticExpressionS parsed) (Env globs [])

emptyProgram :: SemProgram
emptyProgram = SemProgram [] []

-- Unit tests

test :: String -> Either CompilerError SemExpression
test s = parseExpression s >>= createSemanticExpression

testT :: String -> Either CompilerError SemType
testT s = parseType s >>= createSemanticType

testP :: String -> Either CompilerError SemProgram
testP s = parseProgramSingleError s >>= createSemanticProgram

semanticAnalyzerTests :: [Bool]
semanticAnalyzerTests =
  [ test "\\a.a" == Right (SLambda "a" (SId $ Identifier 1 "a")),
    test "\\a.\\b.a b" == Right (SLambda "a" (SLambda "b" (SApplication (SId $ Identifier 2 "a") (SId $ Identifier 1 "b")))),
    test "(\\a.a) x" == Left (SemanticError $ SUndefinedVariable "x"),
    testT "int" == Right (STypeId TInteger),
    testT "int -> int" == Right (SFunctionType (STypeId TInteger) (STypeId TInteger)),
    testT "(int -> int) -> int" == Right (SFunctionType (SFunctionType (STypeId TInteger) (STypeId TInteger)) (STypeId TInteger)),
    testP program1 == Right program1ShouldBe,
    testP program2 == Right program2ShouldBe,
    testP program3 == Left (SemanticError $ SUndefinedVariable "notE"),
    parseAndCreateProgram program1 == Right program1ShouldBe,
    parseAndCreateProgram program2 == Right program2ShouldBe,
    (parseAndCreateProgram program1 >>= (`parseAndCreateExpressionWithProgram` "true")) == Right (SRef "true")
  ]

program1 :: String
program1 =
  "true := \\a.\\b.a\n"
    ++ "false := \\a.\\b.b"

program1ShouldBe :: SemProgram
program1ShouldBe =
  SemProgram
    ["false", "true"]
    [ SemDefinition "true" (SLambda "a" (SLambda "b" (SId $ Identifier 2 "a"))),
      SemDefinition "false" (SLambda "a" (SLambda "b" (SId $ Identifier 1 "b")))
    ]

program2 :: String
program2 =
  "true := \\a.\\b.a\n"
    ++ "false := \\a.\\b.b\n"
    ++ "and := \\a.\\b. a b false\n"
    ++ "or := \\a.\\b. a true b\n"
    ++ "not := \\a. a false true\n"
    ++ "xor := \\a.\\b. a (not b) b"

program2ShouldBe :: SemProgram
program2ShouldBe =
  SemProgram
    ["xor", "not", "or", "and", "false", "true"]
    [ SemDefinition "true" (SLambda "a" (SLambda "b" (SId $ Identifier 2 "a"))),
      SemDefinition "false" (SLambda "a" (SLambda "b" (SId $ Identifier 1 "b"))),
      SemDefinition "and" (SLambda "a" (SLambda "b" (SApplication (SApplication (SId $ Identifier 2 "a") (SId $ Identifier 1 "b")) (SRef "false")))),
      SemDefinition "or" (SLambda "a" (SLambda "b" (SApplication (SApplication (SId $ Identifier 2 "a") (SRef "true")) (SId $ Identifier 1 "b")))),
      SemDefinition "not" (SLambda "a" (SApplication (SApplication (SId $ Identifier 1 "a") (SRef "false")) (SRef "true"))),
      SemDefinition "xor" (SLambda "a" (SLambda "b" (SApplication (SApplication (SId $ Identifier 2 "a") (SApplication (SRef "not") (SId $ Identifier 1 "b"))) (SId $ Identifier 1 "b"))))
    ]

program3 :: String
program3 =
  "true := \\a.\\b.a\n"
    ++ "false := \\a.\\b.b\n"
    ++ "and := \\a.\\b. a b false\n"
    ++ "or := \\a.\\b. a true b\n"
    ++ "not := \\a. a false true\n"
    ++ "xor := \\a.\\b. a (notE b) b"
