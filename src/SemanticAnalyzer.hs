{-# LANGUAGE TupleSections #-}

module SemanticAnalyzer
  ( SemanticError (..),
    Expression (..),
    Program (..),
    Definition (..),
    State (..),
    createSemanticProgram,
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
import qualified Lexer as L
import SemanticAnalyzer.Expression
import SemanticAnalyzer.Type
import qualified SyntacticAnalyzer as Y
import Util

data Definition = Definition
  { defName :: L.Ident,
    defExpr :: InfExpr
  }
  deriving (Eq)

data Program = Program
  { progGlobals :: [L.Ident],
    progDefs :: [Definition]
  }
  deriving (Eq, Show)

instance Show Definition where
  show (Definition name infExpr) = L.unIdent name ++ " := " ++ show expr

type SourceCode = String

-- Type-inferred expression with dependencies
data DepInfExpr = DepInfExpr
  { dieExpr :: Expression,
    dieType :: Type,
    dieGlobEnv :: [Definition],
    dieDeps :: [(L.Ident, NormType)]
  }

-- Type-inferred expression
data InfExpr = InfExpr
  { ieExpr :: Expression,
    ieType :: NormType
  }
  deriving (Eq)

createSemanticPartsS :: Y.Program -> State ConvertEnv (Either CompilerError [Definition])
createSemanticPartsS = foldEitherM (flip helper) []
  where
    foldEitherM :: (Monad m) => (a -> b -> m (Either e b)) -> b -> [a] -> m (Either e b)
    foldEitherM _ acc [] = return $ Right acc
    foldEitherM f acc (x : xs) = do
      res <- f x acc
      case res of
        Left err -> return $ Left err
        Right newAcc -> foldEitherM f newAcc xs
    helper :: [Definition] -> Y.ProgramPart -> State ConvertEnv (Either CompilerError [Definition])
    helper parts (Y.Declaration name typ) = do
      let conTyp = convertType typ
      addDecl name conTyp
      return $ Right parts
    helper parts (Y.Definition name expr) = do
      semExpr <- convertExpressionS expr
      let typM = mkDepInfExpr parts semExpr
      case typM of
        Left e -> return $ Left $ SemanticError $ STypeError e
        Right (typ, newGlobalInfers) -> do
          errE <- mergeWithCurrentGlobalInfers newGlobalInfers
          case errE of
            Left e -> return $ Left $ SemanticError $ STypeError e
            Right _ -> do
              addGlobal name
              return $ Right $ parts ++ [Definition name semExpr typ]

createSemanticProgram :: Y.Program -> Either CompilerError Program
createSemanticProgram prog = Program globs <$> (semProgParts >>= sinkError . checkSemProgParts declarations)
  where
    (ConvertEnv globs _ declarations _, semProgParts) =
      runState (createSemanticPartsS prog) $
        ConvertEnv [] [] [] []
    sinkError :: Either STypeError a -> Either CompilerError a
    sinkError (Left e) = Left $ SemanticError $ STypeError e
    sinkError (Right a) = Right a

checkSemProgParts :: [(L.Ident, NormType)] -> [Definition] -> Either STypeError [Definition]
checkSemProgParts declarations =
  mapM
    ( \(Definition name infexpr) -> case lookup name declarations of
        Nothing -> Right $ Definition name expr typ
        Just declTyp -> case checkType declTyp typ of
          Left e -> Left e
          Right _ -> Right $ Definition name expr declTyp
    )

parseAndCreateProgram :: SourceCode -> Either CompilerError Program
parseAndCreateProgram s = parseProgramSingleError s >>= createSemanticProgram

parseAndCreateExpressionWithProgram :: Program -> SourceCode -> Either CompilerError Expression
parseAndCreateExpressionWithProgram (Program globs _) s = do
  parsed <- parseExpression s
  return $ execState (createSemanticExpressionS parsed) (Env globs [] [] [])

lookupRefExp :: [Definition] -> L.Ident -> Maybe Expression
lookupRefExp parts refName =
  find
    ( \(Definition name _) -> name == refName
    )
    parts
    >>= \(Definition _ expr) -> Just expr

lookupRefType :: [Definition] -> L.Ident -> Maybe NormType
lookupRefType parts refName =
  find
    ( \(Definition name _) -> name == refName
    )
    parts
    >>= \(Definition _ _) -> Just typ

mergeGlobalInfers ::
  [(L.Ident, NormType)] ->
  [(L.Ident, NormType)] ->
  Either STypeError [(L.Ident, NormType)]
mergeGlobalInfers [] oldInfers = Right oldInfers
mergeGlobalInfers ((name, typ) : newInfers) oldInfers =
  case lookup name oldInfers of
    Nothing -> ((name, typ) :) <$> mergeGlobalInfers newInfers oldInfers
    Just oldTyp -> do
      let meTyps = mkMutExcTy2 oldTyp typ
      mergedTyp <-
        execState
          ( reconcileTypesS
              (ntType $ met2Fst meTyps)
              (met2Snd meTyps)
          )
          (ReconcileEnv [])
      ((name, mkNormType mergedTyp) :) <$> mergeGlobalInfers newInfers oldInfers

mergeWithCurrentGlobalInfers :: [(L.Ident, NormType)] -> State ConvertEnv (Either STypeError ())
mergeWithCurrentGlobalInfers newInfers = do
  currEnv@(ConvertEnv _ _ _ globalInferences) <- get
  let mergedE = mergeGlobalInfers newInfers globalInferences
  case mergedE of
    Left e -> return $ Left e
    Right merged -> do
      put $ currEnv {globalInfers = merged}
      return $ Right ()

emptyProgram :: Program
emptyProgram = Program [] []

mkDepInfExprS :: [Definition] -> Expression -> State InferEnv (Either STypeError (Type, [Type]))
mkDepInfExprS _ (Lit (Y.IntegerLiteral _)) = return $ Right (AtomicType AInt, [])
mkDepInfExprS _ (Ident ident) = do
  (generics, lastGeneric) <- createGenericList ident
  return $ Right (lastGeneric, generics)
mkDepInfExprS parts (Ref refName) = do
  case lookupRefType parts refName of
    Nothing -> do
      typ <- getTypeInferredToRefSI refName
      return $ Right (typ, [])
    Just typ -> do
      shifted <- shiftNewIds $ ntType typ
      return $ Right (shifted, [])
mkDepInfExprS parts (Lambda _ expr) = do
  inferred <- mkDepInfExprS parts expr
  case inferred of
    Left e -> return $ Left e
    Right (exprType, exprGenerics) -> do
      case exprGenerics of
        [] -> do
          paramId <- getNewId
          return $ Right (FunctionType (GenericType paramId) exprType, [])
        (paramType : otherGenerics) -> do
          return $ Right (FunctionType paramType exprType, otherGenerics)
mkDepInfExprS parts (Application expr1 expr2) = do
  inferred1 <- mkDepInfExprS parts expr1
  inferred2 <- mkDepInfExprS parts expr2
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
            FunctionType paramType returnType -> do
              updatedExpr2Type <- updateWithSubstitutionsI expr2Type
              updatedParam <- updateWithSubstitutionsI paramType
              reconciledParamM <- reconcileTypesIS updatedParam updatedExpr2Type
              case reconciledParamM of
                Left e -> return $ Left e
                Right _ -> do
                  updatedReturn <- updateWithSubstitutionsI returnType
                  return $ Right (updatedReturn, updatedGenerics)
            GenericType genericId -> do
              updatedExpr2Type <- updateWithSubstitutionsI expr2Type
              newReturnId <- getNewId
              addingWorked <-
                addNewSubstitutionI
                  genericId
                  (FunctionType updatedExpr2Type (GenericType newReturnId))
              case addingWorked of
                Left e -> return $ Left e
                Right _ -> do
                  updatedGenerics' <- mapM updateWithSubstitutionsI updatedGenerics
                  return $ Right (GenericType newReturnId, updatedGenerics')
            typ -> return $ Left $ STApplyingToANonFunction $ show typ

mkDepInfExpr :: [Definition] -> Expression -> Either STypeError DepInfExpr
mkDepInfExpr parts expr = (,normRefs) <$> normTypeE
  where
    (ienv, typE) = runState (mkDepInfExprS parts expr) (InferEnv 1 (ReconcileEnv []) [])
    normTypeE = mkNormType . fst <$> typE
    refs = execState getAllTypesInferredToRefsSI ienv
    normRefs = second mkNormType <$> refs

mkInfExpr :: [Definition] -> Expression -> Either STypeError InfExpr
mkInfExpr parts expr = do
  (normType, refs) <- mkDepInfExpr parts expr
  return $ InfExpr expr normType refs

-- Unit tests

test :: SourceCode -> Either CompilerError Expression
test s = convertExpression <$> Y.parseExpression s

testT :: SourceCode -> Either CompilerError NormType
testT s = convertType <$> Y.parseType s

testP :: SourceCode -> Either CompilerError Program
testP s = Y.parseProgramSingleError s >>= createSemanticProgram

-- Unit tests

nai :: Type
nai = AtomicType AInt

lam :: String -> Expression -> Expression
lam s = Lambda (L.Ident s)

funt :: Type -> Type -> Type
funt = FunctionType

mkn :: Type -> NormType
mkn = mkNormType

ident :: String -> L.Ident
ident = L.Ident

defi :: String -> Expression -> Definition
defi s = Definition (ident s)

semanticAnalyzerTests :: [Bool]
semanticAnalyzerTests =
  [ test "\\a.a" == Right (lam "a" (Ident 1)),
    test "\\a.\\b.a b" == Right (lam "a" (lam "b" (Application (Ident 2) (Ident 1)))),
    test "(\\a.a) x" == Left (SemanticError $ SUndefinedVariable "x"),
    testT "int" == Right (mkn nai),
    testT "int -> int" == Right (mkn $ funt nai nai),
    testT "(int -> int) -> int" == Right (mkn $ funt (funt nai nai) nai),
    testP program1 == Right program1ShouldBe,
    testP program2 == Right program2ShouldBe,
    testP program3 == Left (SemanticError $ SUndefinedVariable "notE"),
    parseAndCreateProgram program1 == Right program1ShouldBe,
    parseAndCreateProgram program2 == Right program2ShouldBe,
    (parseAndCreateProgram program1 >>= (`parseAndCreateExpressionWithProgram` "true")) == Right (Ref $ ident "true"),
    parseAndCreateProgram program4 == Right program4ShouldBe,
    parseAndCreateProgram program5 == Right program5ShouldBe,
    parseAndCreateProgram program6 == Right program6ShouldBe
  ]

program1 :: SourceCode
program1 =
  "true := \\a.\\b.a\n"
    ++ "false := \\a.\\b.b"

program1ShouldBe :: Program
program1ShouldBe =
  Program
    [ident "true", ident "false"]
    [ Definition "true" (lam "a" (lam "b" (Ident 2))) (mkn $ funt (GenericType 1) (funt (GenericType 2) (GenericType 1))),
      Definition "false" (lam "a" (lam "b" (Ident 1))) (mkn $ funt (GenericType 1) (funt (GenericType 2) (GenericType 2)))
    ]

program2 :: SourceCode
program2 =
  "true := \\a.\\b.a\n"
    ++ "false := \\a.\\b.b\n"
    ++ "and := \\a.\\b. a b false\n"
    ++ "or := \\a.\\b. a true b\n"
    ++ "not := \\a. a false true\n"
    ++ "xor := \\a.\\b. a (not b) b"

program2ShouldBe :: Program
program2ShouldBe =
  Program
    [ident "true", ident "false", ident "and", ident "or", ident "not", ident "xor"]
    [ Definition (ident "true") (lam "a" (lam "b" (Ident 2))) (funt (GenericType 1) (funt (GenericType 2) (GenericType 1))),
      Definition "false" (lam "a" (lam "b" (Ident 1))) (funt (GenericType 1) (funt (GenericType 2) (GenericType 2))),
      Definition "and" (lam "a" (lam "b" (Application (Application (Ident 2) (Ident 1)) (Ref "false")))) (funt (funt (GenericType 1) (funt (funt (GenericType 2) (funt (GenericType 3) (GenericType 3))) (GenericType 4))) (funt (GenericType 1) (GenericType 4))),
      Definition "or" (lam "a" (lam "b" (Application (Application (Ident 2) (Ref "true")) (Ident 1)))) (funt (funt (funt (GenericType 1) (funt (GenericType 2) (GenericType 1))) (funt (GenericType 3) (GenericType 4))) (funt (GenericType 3) (GenericType 4))),
      Definition "not" (lam "a" (Application (Application (Ident 1) (Ref "false")) (Ref "true"))) (funt (funt (funt (GenericType 1) (funt (GenericType 2) (GenericType 2))) (funt (funt (GenericType 3) (funt (GenericType 4) (GenericType 3))) (GenericType 5))) (GenericType 5)),
      Definition "xor" (lam "a" (lam "b" (Application (Application (Ident 2) (Application (Ref "not") (Ident 1))) (Ident 1)))) (funt (funt (GenericType 1) (funt (funt (funt (GenericType 2) (funt (GenericType 3) (GenericType 3))) (funt (funt (GenericType 4) (funt (GenericType 5) (GenericType 4))) (GenericType 1))) (GenericType 6))) (funt (funt (funt (GenericType 2) (funt (GenericType 3) (GenericType 3))) (funt (funt (GenericType 4) (funt (GenericType 5) (GenericType 4))) (GenericType 1))) (GenericType 6)))
    ]

program3 :: SourceCode
program3 =
  "true := \\a.\\b.a\n"
    ++ "false := \\a.\\b.b\n"
    ++ "and := \\a.\\b. a b false\n"
    ++ "or := \\a.\\b. a true b\n"
    ++ "not := \\a. a false true\n"
    ++ "xor := \\a.\\b. a (notE b) b"

program4 :: SourceCode
program4 =
  "te := \\a. \\b. a b\n"

program4ShouldBe :: Program
program4ShouldBe =
  Program
    ["te"]
    [Definition "te" (lam "a" (lam "b" (Application (Ident 2) (Ident 1)))) (funt (funt (GenericType 1) (GenericType 2)) (funt (GenericType 1) (GenericType 2)))]

program5 :: SourceCode
program5 =
  "zero := \\f. \\z. z\n"
    ++ "succ := \\n. \\f. \\z. f (n f z)\n"
    ++ "one := succ zero\n"
    ++ "two := succ one\n"
    ++ "three := succ two\n"

program5ShouldBe :: Program
program5ShouldBe =
  Program
    ["zero", "succ", "one", "two", "three"]
    [ Definition "zero" (lam "f" (lam "z" (Ident 1))) (funt (GenericType 1) (funt (GenericType 2) (GenericType 2))),
      Definition "succ" (lam "n" (lam "f" (lam "z" (Application (Ident 2) (Application (Application (Ident 3) (Ident 2)) (Ident 1)))))) (funt (funt (funt (GenericType 1) (GenericType 2)) (funt (GenericType 3) (GenericType 1))) (funt (funt (GenericType 1) (GenericType 2)) (funt (GenericType 3) (GenericType 2)))),
      Definition "one" (Application (Ref "succ") (Ref "zero")) (funt (funt (GenericType 1) (GenericType 2)) (funt (GenericType 1) (GenericType 2))),
      Definition "two" (Application (Ref "succ") (Ref "one")) (funt (funt (GenericType 1) (GenericType 1)) (funt (GenericType 1) (GenericType 1))),
      Definition "three" (Application (Ref "succ") (Ref "two")) (funt (funt (GenericType 1) (GenericType 1)) (funt (GenericType 1) (GenericType 1)))
    ]

program6ShouldBe :: Program
program6ShouldBe =
  Program
    ["true", "false"]
    [ Definition "true" (lam "a" (lam "b" (Ident 2))) (funt (GenericType 1) (funt (GenericType 1) (GenericType 1))),
      Definition "false" (lam "a" (lam "b" (Ident 1))) (funt (GenericType 1) (funt (GenericType 1) (GenericType 1)))
    ]

program6 :: SourceCode
program6 =
  "true : a -> a -> a\n"
    ++ "true := \\a.\\b.a\n"
    ++ "false : a -> a -> a\n"
    ++ "false := \\a.\\b.b\n"

at = \x -> bt (x + 1 :: Int)

bt = \x -> at x
