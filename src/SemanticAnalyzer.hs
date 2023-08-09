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

import Control.Monad
import Data.Bifunctor
import Data.Foldable
import Errors
import SemanticAnalyzer.SemExpression
import SyntacticAnalyzer
import Util

data AtomicType = AInt deriving (Show, Eq)

data SemType
  = SAtomicType AtomicType
  | SGenericType Int
  | SFunctionType SemType SemType
  deriving (Eq)

data SemTypeIdentifier = TInteger deriving (Show, Eq)

data Env = Env {globals :: [String], scope :: [String], decls :: [(String, SemType)]} deriving (Eq, Show)

data SemProgramPart = SemDefinition String SemExpression SemType deriving (Eq)

data SemProgram = SemProgram [String] [SemProgramPart] deriving (Eq, Show)

instance Show SemProgramPart where
  show (SemDefinition name expr typ) = name ++ " := " ++ show expr ++ " : " ++ show typ

instance Show SemType where
  show (SAtomicType a) = show a
  show (SGenericType n) = "T" ++ show n
  show (SFunctionType t1@(SFunctionType _ _) t2) = "(" ++ show t1 ++ ") -> " ++ show t2
  show (SFunctionType t1 t2) = show t1 ++ " -> " ++ show t2

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

createSemanticType :: SynTypeExpression -> Either CompilerError SemType
createSemanticType (TypeId "int") = Right $ SAtomicType AInt
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

data InferEnv = InferEnv Int ReconcileEnv

getNewId :: State InferEnv Int
getNewId = do
  InferEnv ident rec <- get
  put $ InferEnv (ident + 1) rec
  return ident

shiftNewIds :: SemType -> State InferEnv SemType
shiftNewIds typ = do
  InferEnv ident rec <- get
  let (newTyp, maxIdent) = shiftIds ident typ
  put $ InferEnv maxIdent rec
  return newTyp
  where
    shiftIds :: Int -> SemType -> (SemType, Int)
    shiftIds ident (SAtomicType a) = (SAtomicType a, ident)
    shiftIds ident (SFunctionType t1 t2) =
      let (newT1, maxIdent1) = shiftIds ident t1
          (newT2, maxIdent2) = shiftIds ident t2
       in (SFunctionType newT1 newT2, max maxIdent1 maxIdent2)
    shiftIds ident (SGenericType gid) = (SGenericType (gid + ident - 1), gid + ident)

-- Also returns the last element of the list
createGenericList :: Int -> State InferEnv ([SemType], SemType)
createGenericList 0 = error "createGenericList called with 0"
createGenericList n = do
  start <- replicateM (n - 1) (SGenericType <$> getNewId)
  lastId <- getNewId
  return (start ++ [SGenericType lastId], SGenericType lastId)

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

-- Laws
--   1. forall (a, _) in RE : forall (_, b) in RE : forall genericId in b : a != genericId
--   2. forall (a, b), (a', b') in RE : b != b' => a != a'
newtype ReconcileEnv = ReconcileEnv [(Int, SemType)]

addNewSubstitution :: Int -> SemType -> State ReconcileEnv (Either STypeError ())
addNewSubstitution genericId typ = do
  ReconcileEnv env <- get
  let substituted = checkAndSubstitute env typ
  if checkSelfRefs genericId substituted
    then return $ Left $ STSelfReferenceFound genericId $ show substituted
    else do
      let newEnv = substituteOldSubs genericId substituted env
      put $ ReconcileEnv ((genericId, substituted) : newEnv)
      return $ Right ()
  where
    -- 1. Check for and substitute references in the new substitution
    checkAndSubstitute :: [(Int, SemType)] -> SemType -> SemType
    checkAndSubstitute subs (SGenericType genericId') = case lookup genericId' subs of
      Nothing -> SGenericType genericId'
      Just typ' -> typ'
    checkAndSubstitute _ (SAtomicType atomicType) = SAtomicType atomicType
    checkAndSubstitute subs (SFunctionType paramType returnType) =
      SFunctionType (checkAndSubstitute subs paramType) (checkAndSubstitute subs returnType)
    -- 2. Check for self references in the new substitution (fail if found)
    checkSelfRefs :: Int -> SemType -> Bool
    checkSelfRefs genericId' (SGenericType genericId'') = genericId' == genericId''
    checkSelfRefs _ (SAtomicType _) = False
    checkSelfRefs genericId' (SFunctionType paramType returnType) =
      checkSelfRefs genericId' paramType || checkSelfRefs genericId' returnType
    -- 3. Check for and substitute references in the old substitutions
    substitueSimple :: Int -> SemType -> SemType -> SemType
    substitueSimple genericId' typ' (SGenericType genericId'') =
      if genericId' == genericId''
        then typ'
        else SGenericType genericId''
    substitueSimple _ _ (SAtomicType atomicType) = SAtomicType atomicType
    substitueSimple genericId' typ' (SFunctionType paramType returnType) =
      SFunctionType (substitueSimple genericId' typ' paramType) (substitueSimple genericId' typ' returnType)
    substituteOldSubs :: Int -> SemType -> [(Int, SemType)] -> [(Int, SemType)]
    substituteOldSubs genericId' typ' =
      map (second (substitueSimple genericId' typ'))

addNewSubstitutionI :: Int -> SemType -> State InferEnv (Either STypeError ())
addNewSubstitutionI genericId typ = do
  InferEnv count renv <- get
  let (newREnv, result) = runState (addNewSubstitution genericId typ) renv
  put $ InferEnv count newREnv
  return result

updateWithSubstitutions :: SemType -> State ReconcileEnv SemType
updateWithSubstitutions typ = do
  ReconcileEnv env <- get
  return $ updateWithSubstitutions' env typ
  where
    updateWithSubstitutions' :: [(Int, SemType)] -> SemType -> SemType
    updateWithSubstitutions' subs (SGenericType genericId) = case lookup genericId subs of
      Nothing -> SGenericType genericId
      Just typ' -> typ'
    updateWithSubstitutions' _ (SAtomicType atomicType) = SAtomicType atomicType
    updateWithSubstitutions' subs (SFunctionType paramType returnType) =
      SFunctionType (updateWithSubstitutions' subs paramType) (updateWithSubstitutions' subs returnType)

updateWithSubstitutionsI :: SemType -> State InferEnv SemType
updateWithSubstitutionsI typ = do
  InferEnv count renv <- get
  let (newREnv, newTyp) = runState (updateWithSubstitutions typ) renv
  put $ InferEnv count newREnv
  return newTyp

reconcileTypesS :: SemType -> SemType -> State ReconcileEnv (Either STypeError SemType)
reconcileTypesS (SGenericType genericId) typ = do
  added <- addNewSubstitution genericId typ
  case added of
    Left e -> return $ Left e
    Right _ -> return $ Right typ
reconcileTypesS typ (SGenericType genericId) = do
  added <- addNewSubstitution genericId typ
  case added of
    Left e -> return $ Left e
    Right _ -> return $ Right typ
reconcileTypesS (SAtomicType atomicType) (SAtomicType atomicType') =
  if atomicType == atomicType'
    then return $ Right (SAtomicType atomicType)
    else return $ Left $ STAtomicTypeMismatch (show atomicType) (show atomicType')
reconcileTypesS
  (SFunctionType paramType returnType)
  (SFunctionType paramType' returnType') = do
    reconciledParam <- reconcileTypesS paramType paramType'
    updatedReturn <- updateWithSubstitutions returnType
    updatedReturn' <- updateWithSubstitutions returnType'
    reconciledReturn <- reconcileTypesS updatedReturn updatedReturn'
    case (reconciledParam, reconciledReturn) of
      (Left e, _) -> return $ Left e
      (_, Left e) -> return $ Left e
      (Right reconciledParam', Right reconciledReturn') ->
        return $ Right (SFunctionType reconciledParam' reconciledReturn')
reconcileTypesS typ typ' = return $ Left $ STTypeMismatch (show typ) (show typ')

reconcileTypesIS :: SemType -> SemType -> State InferEnv (Either STypeError SemType)
reconcileTypesIS t1 t2 = do
  InferEnv count renv <- get
  let (newREnv, reconciledM) = runState (reconcileTypesS t1 t2) renv
  case reconciledM of
    Left e -> return $ Left e
    Right reconciled -> do
      put $ InferEnv count newREnv
      return $ Right reconciled

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
    (parseAndCreateProgram program1 >>= (`parseAndCreateExpressionWithProgram` "true")) == Right (SRef "true")
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
      SemDefinition "and" (SLambda "a" (SLambda "b" (SApplication (SApplication (SId 2) (SId 1)) (SRef "false")))) (SAtomicType AInt),
      SemDefinition "or" (SLambda "a" (SLambda "b" (SApplication (SApplication (SId 2) (SRef "true")) (SId 1)))) (SAtomicType AInt),
      SemDefinition "not" (SLambda "a" (SApplication (SApplication (SId 1) (SRef "false")) (SRef "true"))) (SAtomicType AInt),
      SemDefinition "xor" (SLambda "a" (SLambda "b" (SApplication (SApplication (SId 2) (SApplication (SRef "not") (SId 1))) (SId 1)))) (SAtomicType AInt)
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
