{-# HLINT ignore "Use tuple-section" #-}
{-# HLINT ignore "Use execState" #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wincomplete-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module SemanticAnalyzer
  ( SemanticError (..),
    SemExpression (..),
    SemProgram (..),
    SemProgramPart (..),
    Env (..),
    State (..),
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

import Data.Bifunctor
import Data.Foldable
import Errors
import SemanticAnalyzer.SemExpression
import SemanticAnalyzer.SemType
import SyntacticAnalyzer
import Util

data SemProgramPart = SemDefinition String SemExpression SemType deriving (Eq)

data SemProgram = SemProgram [String] [SemProgramPart] deriving (Eq, Show)

instance Show SemProgramPart where
  show (SemDefinition name expr typ) = name ++ " := " ++ show expr ++ " : " ++ show typ

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
    helper parts (SynDeclaration name typ) = do
      let semType = createSemanticType typ
      addDecl name semType
      return $ Right parts
    helper parts (SynDefinition name expr) = do
      semExpr <- createSemanticExpressionS expr
      let typM = inferType parts semExpr
      case typM of
        Left e -> return $ Left $ SemanticError $ STypeError e
        Right (typ, newGlobalInfers) -> do
          mergeWithCurrentGlobalInfers newGlobalInfers
          addGlobal name
          return $ Right $ parts ++ [SemDefinition name semExpr typ]

createSemanticProgram :: Program -> Either CompilerError SemProgram
createSemanticProgram prog = SemProgram globs <$> (semProgParts >>= sinkError . checkSemProgParts declarations)
  where
    (Env globs _ declarations _, semProgParts) = runState (createSemanticPartsS prog) startingEnv
    sinkError :: Either STypeError a -> Either CompilerError a
    sinkError (Left e) = Left $ SemanticError $ STypeError e
    sinkError (Right a) = Right a

checkSemProgParts :: [(String, SemType)] -> [SemProgramPart] -> Either STypeError [SemProgramPart]
checkSemProgParts declarations =
  mapM
    ( \(SemDefinition name expr typ) -> case lookup name declarations of
        Nothing -> Right $ SemDefinition name expr typ
        Just declTyp -> case checkType declTyp typ of
          Left e -> Left e
          Right _ -> Right $ SemDefinition name expr declTyp
    )

parseAndCreateProgram :: String -> Either CompilerError SemProgram
parseAndCreateProgram s = parseProgramSingleError s >>= createSemanticProgram

parseAndCreateExpressionWithProgram :: SemProgram -> String -> Either CompilerError SemExpression
parseAndCreateExpressionWithProgram (SemProgram globs _) s = do
  parsed <- parseExpression s
  return $ execState (createSemanticExpressionS parsed) (Env globs [] [] [])

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

mergeGlobalInfers :: [(String, SemType)] -> [(String, SemType)] -> Either STypeError [(String, SemType)]
mergeGlobalInfers [] oldInfers = Right oldInfers
mergeGlobalInfers ((name, typ) : newInfers) oldInfers =
  case lookup name oldInfers of
    Nothing -> ((name, typ) :) <$> mergeGlobalInfers newInfers oldInfers
    Just oldTyp -> do
      let shiftedTyp = shiftAwayFrom oldTyp typ
      mergedTyp <- execState (reconcileTypesS shiftedTyp oldTyp) (ReconcileEnv [])
      ((name, mergedTyp) :) <$> mergeGlobalInfers newInfers oldInfers

mergeWithCurrentGlobalInfers :: [(String, SemType)] -> State Env (Either STypeError [(String, SemType)])
mergeWithCurrentGlobalInfers newInfers = do
  currEnv@(Env _ _ _ globalInferences) <- get
  let mergedE = mergeGlobalInfers newInfers globalInferences
  case mergedE of
    Left e -> return $ Left e
    Right merged -> do
      put $ currEnv {globalInfers = merged}
      return $ Right merged

emptyProgram :: SemProgram
emptyProgram = SemProgram [] []

inferTypeS :: [SemProgramPart] -> SemExpression -> State InferEnv (Either STypeError (SemType, [SemType]))
inferTypeS _ (SLit (IntegerLiteral _)) = return $ Right (SAtomicType AInt, [])
inferTypeS _ (SId ident) = do
  (generics, lastGeneric) <- createGenericList ident
  return $ Right (lastGeneric, generics)
inferTypeS parts (SRef refName) = do
  case lookupRefType parts refName of
    Nothing -> do
      typ <- getTypeInferredToRefSI refName
      return $ Right (typ, [])
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

inferType :: [SemProgramPart] -> SemExpression -> Either STypeError (SemType, [(String, SemType)])
inferType parts expr = (,normRefs) <$> normTypeE
  where
    (ienv, typE) = runState (inferTypeS parts expr) (InferEnv 1 (ReconcileEnv []) [])
    normTypeE = normalizeGenericIndices . fst <$> typE
    refs = execState getAllTypesInferredToRefsSI ienv
    normRefs = second normalizeGenericIndices <$> refs

-- Unit tests

test :: String -> Either CompilerError SemExpression
test s = createSemanticExpression <$> parseExpression s

testT :: String -> Either CompilerError SemType
testT s = createSemanticType <$> parseType s

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
    parseAndCreateProgram program4 == Right program4ShouldBe,
    parseAndCreateProgram program5 == Right program5ShouldBe,
    parseAndCreateProgram program6 == Right program6ShouldBe
  ]

program1 :: String
program1 =
  "true := \\a.\\b.a\n"
    ++ "false := \\a.\\b.b"

program1ShouldBe :: SemProgram
program1ShouldBe =
  SemProgram
    ["true", "false"]
    [ SemDefinition "true" (SLambda "a" (SLambda "b" (SId 2))) (SFunctionType (SGenericType 1) (SFunctionType (SGenericType 2) (SGenericType 1))),
      SemDefinition "false" (SLambda "a" (SLambda "b" (SId 1))) (SFunctionType (SGenericType 1) (SFunctionType (SGenericType 2) (SGenericType 2)))
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
    [ SemDefinition "true" (SLambda "a" (SLambda "b" (SId 2))) (SFunctionType (SGenericType 1) (SFunctionType (SGenericType 2) (SGenericType 1))),
      SemDefinition "false" (SLambda "a" (SLambda "b" (SId 1))) (SFunctionType (SGenericType 1) (SFunctionType (SGenericType 2) (SGenericType 2))),
      SemDefinition "and" (SLambda "a" (SLambda "b" (SApplication (SApplication (SId 2) (SId 1)) (SRef "false")))) (SFunctionType (SFunctionType (SGenericType 1) (SFunctionType (SFunctionType (SGenericType 2) (SFunctionType (SGenericType 3) (SGenericType 3))) (SGenericType 4))) (SFunctionType (SGenericType 1) (SGenericType 4))),
      SemDefinition "or" (SLambda "a" (SLambda "b" (SApplication (SApplication (SId 2) (SRef "true")) (SId 1)))) (SFunctionType (SFunctionType (SFunctionType (SGenericType 1) (SFunctionType (SGenericType 2) (SGenericType 1))) (SFunctionType (SGenericType 3) (SGenericType 4))) (SFunctionType (SGenericType 3) (SGenericType 4))),
      SemDefinition "not" (SLambda "a" (SApplication (SApplication (SId 1) (SRef "false")) (SRef "true"))) (SFunctionType (SFunctionType (SFunctionType (SGenericType 1) (SFunctionType (SGenericType 2) (SGenericType 2))) (SFunctionType (SFunctionType (SGenericType 3) (SFunctionType (SGenericType 4) (SGenericType 3))) (SGenericType 5))) (SGenericType 5)),
      SemDefinition "xor" (SLambda "a" (SLambda "b" (SApplication (SApplication (SId 2) (SApplication (SRef "not") (SId 1))) (SId 1)))) (SFunctionType (SFunctionType (SGenericType 1) (SFunctionType (SFunctionType (SFunctionType (SGenericType 2) (SFunctionType (SGenericType 3) (SGenericType 3))) (SFunctionType (SFunctionType (SGenericType 4) (SFunctionType (SGenericType 5) (SGenericType 4))) (SGenericType 1))) (SGenericType 6))) (SFunctionType (SFunctionType (SFunctionType (SGenericType 2) (SFunctionType (SGenericType 3) (SGenericType 3))) (SFunctionType (SFunctionType (SGenericType 4) (SFunctionType (SGenericType 5) (SGenericType 4))) (SGenericType 1))) (SGenericType 6)))
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
    [SemDefinition "te" (SLambda "a" (SLambda "b" (SApplication (SId 2) (SId 1)))) (SFunctionType (SFunctionType (SGenericType 1) (SGenericType 2)) (SFunctionType (SGenericType 1) (SGenericType 2)))]

program5 :: String
program5 =
  "zero := \\f. \\z. z\n"
    ++ "succ := \\n. \\f. \\z. f (n f z)\n"
    ++ "one := succ zero\n"
    ++ "two := succ one\n"
    ++ "three := succ two\n"

program5ShouldBe :: SemProgram
program5ShouldBe =
  SemProgram
    ["zero", "succ", "one", "two", "three"]
    [ SemDefinition "zero" (SLambda "f" (SLambda "z" (SId 1))) (SFunctionType (SGenericType 1) (SFunctionType (SGenericType 2) (SGenericType 2))),
      SemDefinition "succ" (SLambda "n" (SLambda "f" (SLambda "z" (SApplication (SId 2) (SApplication (SApplication (SId 3) (SId 2)) (SId 1)))))) (SFunctionType (SFunctionType (SFunctionType (SGenericType 1) (SGenericType 2)) (SFunctionType (SGenericType 3) (SGenericType 1))) (SFunctionType (SFunctionType (SGenericType 1) (SGenericType 2)) (SFunctionType (SGenericType 3) (SGenericType 2)))),
      SemDefinition "one" (SApplication (SRef "succ") (SRef "zero")) (SFunctionType (SFunctionType (SGenericType 1) (SGenericType 2)) (SFunctionType (SGenericType 1) (SGenericType 2))),
      SemDefinition "two" (SApplication (SRef "succ") (SRef "one")) (SFunctionType (SFunctionType (SGenericType 1) (SGenericType 1)) (SFunctionType (SGenericType 1) (SGenericType 1))),
      SemDefinition "three" (SApplication (SRef "succ") (SRef "two")) (SFunctionType (SFunctionType (SGenericType 1) (SGenericType 1)) (SFunctionType (SGenericType 1) (SGenericType 1)))
    ]

program6ShouldBe :: SemProgram
program6ShouldBe =
  SemProgram
    ["true", "false"]
    [ SemDefinition "true" (SLambda "a" (SLambda "b" (SId 2))) (SFunctionType (SGenericType 1) (SFunctionType (SGenericType 1) (SGenericType 1))),
      SemDefinition "false" (SLambda "a" (SLambda "b" (SId 1))) (SFunctionType (SGenericType 1) (SFunctionType (SGenericType 1) (SGenericType 1)))
    ]

program6 :: String
program6 =
  "true : a -> a -> a\n"
    ++ "true := \\a.\\b.a\n"
    ++ "false : a -> a -> a\n"
    ++ "false := \\a.\\b.b\n"
