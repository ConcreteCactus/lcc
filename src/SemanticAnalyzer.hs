{-# OPTIONS_GHC -Wincomplete-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use tuple-section" #-}
{-# HLINT ignore "Use execState" #-}

module SemanticAnalyzer
  ( SemanticError (..),
    Identifier (..),
    SemExpression (..),
    SemProgram,
    SemProgramPart (..),
    Env (..),
    State (..),
    startingEnv,
    createSemanticExpression,
    createSemanticExpressionS,
    createSemanticProgram,
    createSemanticProgramS,
  )
where

import Control.Applicative
import Data.Foldable
import qualified Data.List.NonEmpty as Ne
import Errors
import SyntacticAnalyzer
import Util

data Identifier = Identifier {iid :: Integer, iname :: String} deriving (Eq)

data SemTypeIdentifier = TInteger deriving (Show, Eq)

data InferredIdentifier = TIInteger deriving (Show, Eq)

data SemExpression
  = SId Identifier
  | SLit Literal
  | SLambda Identifier SemExpression
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

data Env = Env {scopes :: Ne.NonEmpty [Identifier], variableCounter :: Integer}

data SemProgramPart = SemDeclaration | SemDefinition Identifier SemExpression deriving (Eq)

type SemProgram = [SemProgramPart]

instance Show SemProgramPart where
  show SemDeclaration = "Declaration"
  show (SemDefinition ident expr) = show ident ++ " := " ++ show expr

instance Show SemExpression where
  show (SId identifier) = show identifier
  show (SLit literal) = show literal
  show (SLambda param expr) = "\\" ++ show param ++ "." ++ show expr
  show (SApplication expr1@(SLambda param expr) expr2@(SApplication _ _)) =
    "(" ++ show expr1 ++ ") (" ++ show expr2 ++ ")"
  show (SApplication expr1@(SLambda param expr) expr2) =
    "(" ++ show expr1 ++ ") " ++ show expr2
  show (SApplication expr1 expr2@(SApplication _ _)) =
    show expr1 ++ " (" ++ show expr2 ++ ")"
  show (SApplication expr1 expr2) = show expr1 ++ " " ++ show expr2

instance Show Identifier where
  show (Identifier _ name) = name

startingEnv :: Env
startingEnv = Env (Ne.singleton []) 0

findInMulti :: (Foldable t, Foldable u) => (a -> Bool) -> t (u a) -> Maybe a
findInMulti f = foldr (\a b -> find f a <|> b) Nothing

getVar :: String -> State Env (Maybe Identifier)
getVar name = findInMulti ((== name) . iname) . scopes <$> get

getCount :: State Env Integer
getCount = variableCounter <$> get

addVar :: Identifier -> State Env ()
addVar ident = do
  Env scops count <- get
  let h = Ne.head scops
  let t = Ne.tail scops
  put $ Env ((ident : h) Ne.:| t) count

addVarInc :: String -> State Env Identifier
addVarInc name = do
  count <- getCount
  let ident = Identifier (count + 1) name
  addVar ident
  incCount
  return ident

pushScope :: State Env ()
pushScope = do
  Env scops count <- get
  put $ Env ([] `Ne.cons` scops) count

popScope :: State Env ()
popScope = do
  Env scops count <- get
  put $ Env (Ne.fromList $ Ne.tail scops) count

incCount :: State Env ()
incCount = do
  Env vars count <- get
  put $ Env vars $ count + 1

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
  identM <- getVar name
  case identM of
    Just ident -> return $ Right $ SId ident
    Nothing -> return $ Left (SemanticError $ UndefinedVariable name)
createSemanticExpressionS (Lambda param expr) = do
  pushScope
  paramIdent <- addVarInc param
  sformula <- createSemanticExpressionS expr
  popScope
  return $ SLambda paramIdent <$> sformula
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
  nameIdentM <- getVar name
  case nameIdentM of
    Just ident -> return $ Left (SemanticError ValueRedefinition)
    Nothing -> do
      nameIdent <- addVarInc name
      expr <- createSemanticExpressionS synExpr
      semRest <- createSemanticProgramS rest
      return $ (\e r -> SemDefinition nameIdent e : r) <$> expr <*> semRest

createSemanticProgram :: Program -> Either CompilerError SemProgram
createSemanticProgram prog = snd $ runState (createSemanticProgramS prog) startingEnv

-- Unit tests

test :: String -> Either CompilerError SemExpression
test s = parseExpression s >>= createSemanticExpression

testT :: String -> Either CompilerError SemType
testT s = parseType s >>= createSemanticType

testP :: String -> Either CompilerError SemProgram
testP s = parseProgramSingleError s >>= createSemanticProgram

tests :: [Bool]
tests =
  [ test "\\a.a" == Right (SLambda (Identifier 1 "a") (SId $ Identifier 1 "a")),
    test "\\a.\\b.a b" == Right (SLambda (Identifier 1 "a") (SLambda (Identifier 2 "b") (SApplication (SId $ Identifier 1 "a") (SId $ Identifier 2 "b")))),
    test "(\\a.a) x" == Right (SApplication (SLambda (Identifier 1 "a") (SId $ Identifier 1 "a")) (SId (Identifier 2 "x"))),
    testT "int" == Right (STypeId TInteger),
    testT "int -> int" == Right (SFunctionType (STypeId TInteger) (STypeId TInteger)),
    testT "(int -> int) -> int" == Right (SFunctionType (SFunctionType (STypeId TInteger) (STypeId TInteger)) (STypeId TInteger)),
    testP program1 == Right program1ShouldBe,
    testP program2 == Right program2ShouldBe,
    testP program3 == Left (SemanticError $ UndefinedVariable "notE")
  ]

program1 :: String
program1 =
  "true := \\a.\\b.a\n"
    ++ "false := \\a.\\b.b"

program1ShouldBe :: SemProgram
program1ShouldBe =
  [ SemDefinition (Identifier 1 "true") (SLambda (Identifier 2 "a") (SLambda (Identifier 3 "b") (SId $ Identifier 2 "a"))),
    SemDefinition (Identifier 4 "false") (SLambda (Identifier 5 "a") (SLambda (Identifier 6 "b") (SId $ Identifier 6 "b")))
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
  [ SemDefinition (Identifier 1 "true") (SLambda (Identifier 2 "a") (SLambda (Identifier 3 "b") (SId $ Identifier 2 "a"))),
    SemDefinition (Identifier 4 "false") (SLambda (Identifier 5 "a") (SLambda (Identifier 6 "b") (SId $ Identifier 6 "b"))),
    SemDefinition (Identifier 7 "and") (SLambda (Identifier 8 "a") (SLambda (Identifier 9 "b") (SApplication (SApplication (SId $ Identifier 8 "a") (SId $ Identifier 9 "b")) (SId $ Identifier 4 "false")))),
    SemDefinition (Identifier 10 "or") (SLambda (Identifier 11 "a") (SLambda (Identifier 12 "b") (SApplication (SApplication (SId $ Identifier 11 "a") (SId $ Identifier 1 "true")) (SId $ Identifier 12 "b")))),
    SemDefinition (Identifier 13 "not") (SLambda (Identifier 14 "a") (SApplication (SApplication (SId $ Identifier 14 "a") (SId $ Identifier 4 "false")) (SId $ Identifier 1 "true"))),
    SemDefinition (Identifier 15 "xor") (SLambda (Identifier 16 "a") (SLambda (Identifier 17 "b") (SApplication (SApplication (SId $ Identifier 16 "a") (SApplication (SId $ Identifier 13 "not") (SId $ Identifier 17 "b"))) (SId $ Identifier 17 "b"))))
  ]

program3 :: String
program3 =
  "true := \\a.\\b.a\n"
    ++ "false := \\a.\\b.b\n"
    ++ "and := \\a.\\b. a b false\n"
    ++ "or := \\a.\\b. a true b\n"
    ++ "not := \\a. a false true\n"
    ++ "xor := \\a.\\b. a (notE b) b"
