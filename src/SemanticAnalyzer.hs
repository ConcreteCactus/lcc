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
  put $ env {globals = newGlobal : globals env}

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
            Nothing -> return $ Left $ SemanticError STypeError
            Just typ -> do
              addDecl name typ
              return $ Right $ SemDefinition name semExpr typ : parts

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

-- Also returns the last element of the list
createGenericList :: Int -> State InferEnv ([SemType], SemType)
createGenericList 0 = error "createGenericList called with 0"
createGenericList n = do
  start <- replicateM (n - 1) (SGenericType <$> getNewId)
  lastId <- getNewId
  return (start ++ [SGenericType lastId], SGenericType lastId)

inferTypeS :: [SemProgramPart] -> SemExpression -> State InferEnv (Maybe (SemType, [SemType]))
inferTypeS _ (SLit (IntegerLiteral _)) = return $ Just (SAtomicType AInt, [])
inferTypeS _ (SId ident) = do
  (generics, lastGeneric) <- createGenericList ident
  return $ Just (lastGeneric, generics)
inferTypeS parts (SRef refName) = do
  case lookupRefType parts refName of
    Nothing -> return Nothing
    Just typ -> return $ Just (typ, [])
inferTypeS parts (SLambda _ expr) = do
  inferred <- inferTypeS parts expr
  case inferred of
    Nothing -> return Nothing
    Just (exprType, exprGenerics) -> do
      case exprGenerics of
        [] -> do
          paramId <- getNewId
          return $ Just (SFunctionType (SGenericType paramId) exprType, [])
        (paramType : otherGenerics) -> do
          return $ Just (SFunctionType paramType exprType, otherGenerics)
inferTypeS parts (SApplication expr1 expr2) = do
  inferred1 <- inferTypeS parts expr1
  inferred2 <- inferTypeS parts expr2
  case (inferred1, inferred2) of
    (Nothing, _) -> return Nothing
    (_, Nothing) -> return Nothing
    (Just (expr1Type, expr1Generics), Just (expr2Type, expr2Generics)) -> do
      newGenericsM <- sequence <$> zipWithM reconcileTypesIS expr1Generics expr2Generics
      case newGenericsM of
        Nothing -> return Nothing
        Just newGenerics -> do
          updatedGenerics <- mapM updateWithSubstitutionsI newGenerics
          updatedExpr1Type <- updateWithSubstitutionsI expr1Type
          case updatedExpr1Type of
            SFunctionType paramType returnType -> do
              updatedExpr2Type <- updateWithSubstitutionsI expr2Type
              updatedParam <- updateWithSubstitutionsI paramType
              reconciledParamM <- reconcileTypesIS updatedParam updatedExpr2Type
              case reconciledParamM of
                Nothing -> return Nothing
                Just _ -> do
                  updatedReturn <- updateWithSubstitutionsI returnType
                  return $ Just (updatedReturn, updatedGenerics)
            SGenericType genericId -> do
              updatedExpr2Type <- updateWithSubstitutionsI expr2Type
              newReturnId <- getNewId
              -- This generic id is not used anywhere else, so it's safe to add it and not update
              addingWorked <- addNewSubstitutionI genericId (SFunctionType updatedExpr2Type (SGenericType newReturnId))
              case addingWorked of
                Nothing -> return Nothing
                Just _ ->
                  return $ Just (SGenericType newReturnId, updatedGenerics)
            _ -> return Nothing

inferType :: [SemProgramPart] -> SemExpression -> Maybe SemType
inferType parts expr = fst <$> execState (inferTypeS parts expr) (InferEnv 1 (ReconcileEnv []))

-- Laws
--   1. forall (a, _) in RE : forall (_, b) in RE : forall genericId in b : a != genericId
--   2. forall (a, b), (a', b') in RE : b != b' => a != a'
newtype ReconcileEnv = ReconcileEnv [(Int, SemType)]

addNewSubstitution :: Int -> SemType -> State ReconcileEnv (Maybe ())
addNewSubstitution genericId typ = do
  ReconcileEnv env <- get
  let substitued = checkAndSubstitute env typ
  if checkSelfRefs genericId substitued
    then return Nothing
    else do
      let newEnv = substituteOldSubs genericId substitued env
      put $ ReconcileEnv ((genericId, substitued) : newEnv)
      return $ Just ()
  where
    -- 1. Check for and substitute references in the new substitution
    checkAndSubstitute :: [(Int, SemType)] -> SemType -> SemType
    checkAndSubstitute subs (SGenericType genericId') = case lookup genericId' subs of
      Nothing -> SGenericType genericId
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

addNewSubstitutionI :: Int -> SemType -> State InferEnv (Maybe ())
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

reconcileTypesS :: SemType -> SemType -> State ReconcileEnv (Maybe SemType)
reconcileTypesS (SGenericType genericId) typ = do
  added <- addNewSubstitution genericId typ
  case added of
    Nothing -> return Nothing
    Just _ -> return $ Just typ
reconcileTypesS typ (SGenericType genericId) = do
  added <- addNewSubstitution genericId typ
  case added of
    Nothing -> return Nothing
    Just _ -> return $ Just typ
reconcileTypesS (SAtomicType atomicType) (SAtomicType atomicType') =
  if atomicType == atomicType'
    then return $ Just (SAtomicType atomicType)
    else return Nothing
reconcileTypesS
  (SFunctionType paramType returnType)
  (SFunctionType paramType' returnType') = do
    reconciledParam <- reconcileTypesS paramType paramType'
    updatedReturn <- updateWithSubstitutions returnType
    updatedReturn' <- updateWithSubstitutions returnType'
    reconciledReturn <- reconcileTypesS updatedReturn updatedReturn'
    case (reconciledParam, reconciledReturn) of
      (Nothing, _) -> return Nothing
      (_, Nothing) -> return Nothing
      (Just reconciledParam', Just reconciledReturn') ->
        return $ Just (SFunctionType reconciledParam' reconciledReturn')
reconcileTypesS _ _ = return Nothing

reconcileTypesIS :: SemType -> SemType -> State InferEnv (Maybe SemType)
reconcileTypesIS t1 t2 = do
  InferEnv count renv <- get
  let (newREnv, reconciledM) = runState (reconcileTypesS t1 t2) renv
  case reconciledM of
    Nothing -> return Nothing
    Just reconciled -> do
      put $ InferEnv count newREnv
      return $ Just reconciled

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
    ["false", "true"]
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
    ["xor", "not", "or", "and", "false", "true"]
    [ SemDefinition "true" (SLambda "a" (SLambda "b" (SId 2))) (SFunctionType (SGenericType 1) (SFunctionType (SGenericType 2) (SGenericType 1))),
      SemDefinition "false" (SLambda "a" (SLambda "b" (SId 1))) (SFunctionType (SGenericType 1) (SFunctionType (SGenericType 2) (SGenericType 2))),
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
