{-# OPTIONS_GHC -Wincomplete-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use tuple-section" #-}
{-# HLINT ignore "Use execState" #-}

module SemanticAnalyzer
  ( SemanticError (..),
    SemExpression (..),
    SemProgram (..),
    SemProgramPart (..),
    Env (..),
    State (..),
    startingEnv,
    createSemanticExpression,
    createSemanticExpressionS,
    createSemanticProgram,
    createSemanticPartsS,
    parseAndCreateProgram,
    parseAndCreateExpressionWithProgram,
    emptyProgram,
    lookupRefExp,
    semanticAnalyzerTests,
  )
where

import Data.Foldable
import Errors
import SemanticAnalyzer.SemExpression
import SemanticAnalyzer.SemType
import SyntacticAnalyzer
import Util

data Env = Env {globals :: [String], scope :: [String], decls :: [(String, SemType)]} deriving (Eq, Show)

data SemProgramPart = SemDefinition String SemExpression SemType deriving (Eq)

data SemProgram = SemProgram [String] [SemProgramPart] deriving (Eq, Show)

instance Show SemProgramPart where
  show (SemDefinition name expr typ) = name ++ " := " ++ show expr ++ " : " ++ show typ

startingEnv :: Env
startingEnv = Env [] [] []

addGlobal :: String -> State Env ()
addGlobal newGlobal = do
  env <- get
  put $ env {globals = globals env ++ [newGlobal]}

addVar :: String -> State Env ()
addVar newVar = do
  env <- get
  put $ env {scope = newVar : scope env}

popVar :: State Env ()
popVar = do
  env <- get
  put $ env {scope = tail $ scope env}

findVar :: String -> State Env (Maybe Int)
findVar idName = do
  env <- get
  let scopeVars = scope env
  let (foundNameM, count) = foldr (\name (found, n) -> if name == idName then (Just name, 1) else (found, n + 1)) (Nothing, 0) scopeVars
  case foundNameM of
    Nothing -> return Nothing
    Just _ -> return $ Just count

findGlobal :: String -> State Env (Maybe String)
findGlobal idName = do
  env <- get
  let globalVars = globals env
  let foundNameM = find (== idName) globalVars
  return foundNameM

findDecl :: String -> State Env (Maybe SemType)
findDecl idName =
  lookup idName . decls <$> get

addDecl :: String -> SemType -> State Env ()
addDecl name semType = do
  env <- get
  put $ env {decls = (name, semType) : decls env}

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

createSemanticPartsS :: Program -> State Env (Either CompilerError [SemProgramPart])
createSemanticPartsS = foldEitherM (flip helper) []
  where
    foldEitherM :: (Monad m) => (a -> b -> m (Either e b)) -> b -> [a] -> m (Either e b)
    foldEitherM _ acc [] = return $ Right acc
    foldEitherM f acc (x : xs) = do
      res <- f x acc
      case res of
        Left err -> return $ Left err
        Right newAcc -> foldEitherM f newAcc xs
    helper :: [SemProgramPart] -> SynProgramPart -> State Env (Either CompilerError [SemProgramPart])
    helper parts (SynDeclaration _ _) = return $ Right parts
    helper parts (SynDefinition name expr) = do
      semExprE <- createSemanticExpressionS expr
      case semExprE of
        Left err -> return $ Left err
        Right semExpr -> do
          let typM = inferType parts semExpr
          case typM of
            Left e -> return $ Left $ SemanticError $ STypeError e
            Right typ -> do
              addGlobal name
              return $ Right $ parts ++ [SemDefinition name semExpr typ]

createSemanticProgram :: Program -> Either CompilerError SemProgram
createSemanticProgram prog = SemProgram globs <$> semProgParts
  where
    (Env globs _ _, semProgParts) = runState (createSemanticPartsS prog) startingEnv

parseAndCreateProgram :: String -> Either CompilerError SemProgram
parseAndCreateProgram s = parseProgramSingleError s >>= createSemanticProgram

parseAndCreateExpressionWithProgram :: SemProgram -> String -> Either CompilerError SemExpression
parseAndCreateExpressionWithProgram (SemProgram globs _) s = do
  parsed <- parseExpression s
  execState (createSemanticExpressionS parsed) (Env globs [] [])

lookupRefExp :: [SemProgramPart] -> String -> Maybe SemExpression
lookupRefExp parts refName =
  find
    ( \(SemDefinition name _ _) -> name == refName
    )
    parts
    >>= \(SemDefinition _ expr _) -> Just expr

lookupRefType :: [SemProgramPart] -> String -> Maybe SemType
lookupRefType parts refName =
  find
    ( \(SemDefinition name _ _) -> name == refName
    )
    parts
    >>= \(SemDefinition _ _ typ) -> Just typ

emptyProgram :: SemProgram
emptyProgram = SemProgram [] []

inferTypeS :: [SemProgramPart] -> SemExpression -> State InferEnv (Either STypeError (SemType, [SemType]))
inferTypeS _ (SLit (IntegerLiteral _)) = return $ Right (SAtomicType AInt, [])
inferTypeS _ (SId ident) = do
  (generics, lastGeneric) <- createGenericList ident
  return $ Right (lastGeneric, generics)
inferTypeS parts (SRef refName) = do
  case lookupRefType parts refName of
    Nothing -> return $ Left STReferenceNotFound
    Just typ -> do
      shifted <- shiftNewIds typ
      return $ Right (shifted, [])
inferTypeS parts (SLambda _ expr) = do
  inferred <- inferTypeS parts expr
  case inferred of
    Left e -> return $ Left e
    Right (exprType, exprGenerics) -> do
      case exprGenerics of
        [] -> do
          paramId <- getNewId
          return $ Right (SFunctionType (SGenericType paramId) exprType, [])
        (paramType : otherGenerics) -> do
          return $ Right (SFunctionType paramType exprType, otherGenerics)
inferTypeS parts (SApplication expr1 expr2) = do
  inferred1 <- inferTypeS parts expr1
  inferred2 <- inferTypeS parts expr2
  case (inferred1, inferred2) of
    (Left e, _) -> return $ Left e
    (_, Left e) -> return $ Left e
    (Right (expr1Type, expr1Generics), Right (expr2Type, expr2Generics)) -> do
      newGenericsM <- sequence <$> forgivingZipWithME reconcileTypesIS expr1Generics expr2Generics
      case newGenericsM of
        Left e -> return $ Left e
        Right newGenerics -> do
          updatedGenerics <- mapM updateWithSubstitutionsI newGenerics
          updatedExpr1Type <- updateWithSubstitutionsI expr1Type
          case updatedExpr1Type of
            SFunctionType paramType returnType -> do
              updatedExpr2Type <- updateWithSubstitutionsI expr2Type
              updatedParam <- updateWithSubstitutionsI paramType
              reconciledParamM <- reconcileTypesIS updatedParam updatedExpr2Type
              case reconciledParamM of
                Left e -> return $ Left e
                Right _ -> do
                  updatedReturn <- updateWithSubstitutionsI returnType
                  return $ Right (updatedReturn, updatedGenerics)
            SGenericType genericId -> do
              updatedExpr2Type <- updateWithSubstitutionsI expr2Type
              newReturnId <- getNewId
              addingWorked <-
                addNewSubstitutionI
                  genericId
                  (SFunctionType updatedExpr2Type (SGenericType newReturnId))
              case addingWorked of
                Left e -> return $ Left e
                Right _ -> do
                  updatedGenerics' <- mapM updateWithSubstitutionsI updatedGenerics
                  return $ Right (SGenericType newReturnId, updatedGenerics')
            typ -> return $ Left $ STApplyingToANonFunction $ show typ

inferType :: [SemProgramPart] -> SemExpression -> Either STypeError SemType
inferType parts expr = fst <$> execState (inferTypeS parts expr) (InferEnv 1 (ReconcileEnv []))

-- Unit tests

test :: String -> Either CompilerError SemExpression
test s = parseExpression s >>= createSemanticExpression

testT :: String -> Either CompilerError SemType
testT s = parseType s >>= createSemanticType

testP :: String -> Either CompilerError SemProgram
testP s = parseProgramSingleError s >>= createSemanticProgram

semanticAnalyzerTests :: [Bool]
semanticAnalyzerTests =
  [ test "\\a.a" == Right (SLambda "a" (SId 1)),
    test "\\a.\\b.a b" == Right (SLambda "a" (SLambda "b" (SApplication (SId 2) (SId 1)))),
    test "(\\a.a) x" == Left (SemanticError $ SUndefinedVariable "x"),
    testT "int" == Right (SAtomicType AInt),
    testT "int -> int" == Right (SFunctionType (SAtomicType AInt) (SAtomicType AInt)),
    testT "(int -> int) -> int" == Right (SFunctionType (SFunctionType (SAtomicType AInt) (SAtomicType AInt)) (SAtomicType AInt)),
    testP program1 == Right program1ShouldBe,
    testP program2 == Right program2ShouldBe,
    testP program3 == Left (SemanticError $ SUndefinedVariable "notE"),
    parseAndCreateProgram program1 == Right program1ShouldBe,
    parseAndCreateProgram program2 == Right program2ShouldBe,
    (parseAndCreateProgram program1 >>= (`parseAndCreateExpressionWithProgram` "true")) == Right (SRef "true"),
    parseAndCreateProgram program4 == Right program4ShouldBe
  ]

program1 :: String
program1 =
  "true := \\a.\\b.a\n"
    ++ "false := \\a.\\b.b"

program1ShouldBe :: SemProgram
program1ShouldBe =
  SemProgram
    ["true", "false"]
    [ SemDefinition "true" (SLambda "a" (SLambda "b" (SId 2))) (SFunctionType (SGenericType 2) (SFunctionType (SGenericType 1) (SGenericType 2))),
      SemDefinition "false" (SLambda "a" (SLambda "b" (SId 1))) (SFunctionType (SGenericType 2) (SFunctionType (SGenericType 1) (SGenericType 1)))
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
    ["true", "false", "and", "or", "not", "xor"]
    [ SemDefinition "true" (SLambda "a" (SLambda "b" (SId 2))) (SFunctionType (SGenericType 2) (SFunctionType (SGenericType 1) (SGenericType 2))),
      SemDefinition "false" (SLambda "a" (SLambda "b" (SId 1))) (SFunctionType (SGenericType 2) (SFunctionType (SGenericType 1) (SGenericType 1))),
      SemDefinition "and" (SLambda "a" (SLambda "b" (SApplication (SApplication (SId 2) (SId 1)) (SRef "false")))) (SFunctionType (SFunctionType (SGenericType 3) (SFunctionType (SFunctionType (SGenericType 6) (SFunctionType (SGenericType 5) (SGenericType 5))) (SGenericType 7))) (SFunctionType (SGenericType 3) (SGenericType 7))),
      SemDefinition "or" (SLambda "a" (SLambda "b" (SApplication (SApplication (SId 2) (SRef "true")) (SId 1)))) (SFunctionType (SFunctionType (SFunctionType (SGenericType 4) (SFunctionType (SGenericType 3) (SGenericType 4))) (SFunctionType (SGenericType 6) (SGenericType 7))) (SFunctionType (SGenericType 6) (SGenericType 7))),
      SemDefinition "not" (SLambda "a" (SApplication (SApplication (SId 1) (SRef "false")) (SRef "true"))) (SFunctionType (SFunctionType (SFunctionType (SGenericType 3) (SFunctionType (SGenericType 2) (SGenericType 2))) (SFunctionType (SFunctionType (SGenericType 6) (SFunctionType (SGenericType 5) (SGenericType 6))) (SGenericType 7))) (SGenericType 7)),
      SemDefinition "xor" (SLambda "a" (SLambda "b" (SApplication (SApplication (SId 2) (SApplication (SRef "not") (SId 1))) (SId 1)))) (SFunctionType (SFunctionType (SGenericType 9) (SFunctionType (SFunctionType (SFunctionType (SGenericType 5) (SFunctionType (SGenericType 4) (SGenericType 4))) (SFunctionType (SFunctionType (SGenericType 8) (SFunctionType (SGenericType 7) (SGenericType 8))) (SGenericType 9))) (SGenericType 13))) (SFunctionType (SFunctionType (SFunctionType (SGenericType 5) (SFunctionType (SGenericType 4) (SGenericType 4))) (SFunctionType (SFunctionType (SGenericType 8) (SFunctionType (SGenericType 7) (SGenericType 8))) (SGenericType 9))) (SGenericType 13)))
    ]

program3 :: String
program3 =
  "true := \\a.\\b.a\n"
    ++ "false := \\a.\\b.b\n"
    ++ "and := \\a.\\b. a b false\n"
    ++ "or := \\a.\\b. a true b\n"
    ++ "not := \\a. a false true\n"
    ++ "xor := \\a.\\b. a (notE b) b"

program4 :: String
program4 =
  "te := \\a. \\b. a b\n"

program4ShouldBe :: SemProgram
program4ShouldBe =
  SemProgram
    ["te"]
    [SemDefinition "te" (SLambda "a" (SLambda "b" (SApplication (SId 2) (SId 1)))) (SFunctionType (SFunctionType (SGenericType 3) (SGenericType 4)) (SFunctionType (SGenericType 3) (SGenericType 4)))]
