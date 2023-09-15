{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module SemanticAnalyzer.Internal where

import Data.Bifunctor
import Data.Foldable
import Debug.Trace
import Errors
import qualified Lexer as L
import SemanticAnalyzer.DependencyGraph
import SemanticAnalyzer.Expression
import SemanticAnalyzer.Type
import qualified SyntacticAnalyzer as Y
import Util

data UninfDefinition = UninfDefinition
  { udefName :: L.Ident,
    udefExpr :: Expression
  }
  deriving (Show)

data InfExpr = InfExpr
  { ieExpr :: Expression,
    ieType :: NormType
  }
  deriving (Eq)

instance Show InfExpr where
  show expr = show (ieExpr expr) ++ " : " ++ show (ieType expr)

data TypedExpr = TypedExpr
  { teInfExpr :: InfExpr,
    teType :: NormType
  }
  deriving (Eq)

instance Show TypedExpr where
  show expr = show (teExpr expr) ++ " : " ++ show (teType expr)

data Definition = Definition
  { defName :: L.Ident,
    defExpr :: InfExpr
  }
  deriving (Eq)

instance Show Definition where
  show (Definition name expr) = L.unIdent name ++ " := " ++ show expr

newtype UninfProg = UninfProg [UninfDefinition] deriving (Show)

data ProgInfDeps = ProgInfDeps
  { pidUninfProg :: UninfProg,
    pidDepGraph :: [DependencyGraph L.Ident]
  }

newtype Program = Program
  { progDefs :: [Definition]
  }
  deriving (Eq, Show)

type SourceCode = String

teExpr :: TypedExpr -> Expression
teExpr = ieExpr . teInfExpr

mkUninfProg :: Y.Program -> Either CompilerError UninfProg
mkUninfProg synProg = execState (mkUninfProgS synProg) (ConvertEnv [] [] [] [])

mkUninfProgS :: Y.Program -> State ConvertEnv (Either CompilerError UninfProg)
mkUninfProgS synProg = do
  udefsE <- foldEitherM (flip helper) [] synProg
  case udefsE of
    Left e -> return $ Left e
    Right udefs -> do
      errors <- mapM (checkUndefinedReferencesS . udefExpr) udefs
      case find
        ( \case
            Just _ -> True
            _ -> False
        )
        errors of
        Just (Just e) -> return $ Left e
        Just Nothing -> error "I've got hit by a lightning bolt."
        Nothing -> return $ Right $ UninfProg udefs
  where
    foldEitherM :: (Monad m) => (a -> b -> m (Either e b)) -> b -> [a] -> m (Either e b)
    foldEitherM _ acc [] = return $ Right acc
    foldEitherM f acc (x : xs) = do
      res <- f x acc
      case res of
        Left err -> return $ Left err
        Right newAcc -> foldEitherM f newAcc xs
    helper :: [UninfDefinition] -> Y.ProgramPart -> State ConvertEnv (Either CompilerError [UninfDefinition])
    helper parts (Y.Declaration name typ) = do
      let conTyp = convertType typ
      addDecl name conTyp
      return $ Right parts
    helper parts (Y.Definition name expr) = do
      semExpr <- convertExpressionS expr
      addGlobal name
      return $ Right $ parts ++ [UninfDefinition name semExpr]
    checkUndefinedReferencesS :: Expression -> State ConvertEnv (Maybe CompilerError)
    checkUndefinedReferencesS (Ident _) = return Nothing
    checkUndefinedReferencesS (Lit _) = return Nothing
    checkUndefinedReferencesS (Ref name) = do
      globM <- findGlobal name
      case globM of
        Nothing -> return $ Just $ SemanticError $ SUndefinedVariable (L.unIdent name)
        Just _ -> return Nothing
    checkUndefinedReferencesS (Lambda _ expr) = checkUndefinedReferencesS expr
    checkUndefinedReferencesS (Application expr1 expr2) = do
      c1 <- checkUndefinedReferencesS expr1
      c2 <- checkUndefinedReferencesS expr2
      case (c1, c2) of
        (Just e, _) -> return $ Just e
        (_, Just e) -> return $ Just e
        _ -> return Nothing

mkProgInfDeps :: UninfProg -> Either SemanticError ProgInfDeps
mkProgInfDeps uiprog@(UninfProg uiDefs) = case checkDeps uiprog of
  Just e -> Left e
  Nothing -> Right $ ProgInfDeps uiprog $ mkDependencyGraph (map udefName uiDefs) (getDeps uiprog)
  where
    checkDeps :: UninfProg -> Maybe SemanticError
    checkDeps (UninfProg uiDefs') =
      foldr
        ( \udef acc ->
            ( if all (`elem` map udefName uiDefs') (getAllRefs (udefExpr udef))
                then acc
                else Just $ SUndefinedVariable $ L.unIdent $ udefName udef
            )
        )
        Nothing
        uiDefs'
    getDeps :: UninfProg -> L.Ident -> [L.Ident]
    getDeps (UninfProg uiDefs') glob = case find ((== glob) . udefName) uiDefs' of
      Nothing -> error "Found an undefined global. This is a bug."
      Just definition -> getAllRefs $ udefExpr definition
    getAllRefs :: Expression -> [L.Ident]
    getAllRefs (Ident _) = []
    getAllRefs (Lit _) = []
    getAllRefs (Ref name) = [name]
    getAllRefs (Lambda _ expr) = getAllRefs expr
    getAllRefs (Application expr1 expr2) = getAllRefs expr1 +-+ getAllRefs expr2

mkProgram :: ProgInfDeps -> Either STypeError Program
mkProgram (ProgInfDeps uprog dgraphs) = Program <$> foldr (helperList uprog) (Right []) dgraphs
  where
    helperList :: UninfProg -> DependencyGraph L.Ident -> Either STypeError [Definition] -> Either STypeError [Definition]
    helperList _ _ (Left e) = Left e
    helperList uprog' dgraph (Right defs) = dgFoldr (helperCycle uprog') (helperTree uprog') (Right defs) dgraph
    helperTree :: UninfProg -> L.Ident -> Either STypeError [Definition] -> Either STypeError [Definition]
    helperTree _ _ (Left e) = Left e
    helperTree uprog' a (Right prevDeps) = (: prevDeps) <$> (Definition a <$> mkInfExprTree prevDeps (lookupUDefUnsafe uprog' a))
    helperCycle :: UninfProg -> [L.Ident] -> Either STypeError [Definition] -> Either STypeError [Definition]
    helperCycle _ _ (Left e) = Left e
    helperCycle uprog' as (Right prevDeps) = (++ prevDeps) <$> (zipWith Definition as <$> mkInfExprCycle prevDeps (lookupUDefUnsafe uprog' <$> as))

lookupUDefUnsafe :: UninfProg -> L.Ident -> Expression
lookupUDefUnsafe (UninfProg udefs) sname = case find (\(UninfDefinition name _) -> name == sname) udefs of
  Nothing -> error "A global definition wasn't found."
  Just a -> udefExpr a

mkInfExprTree :: [Definition] -> Expression -> Either STypeError InfExpr
mkInfExprTree defs expr = InfExpr expr . fst <$> execState ((mkNormType `fstMap`) <<$>> infFromExprS defs expr) (InferEnv 1 (ReconcileEnv []) [])

mkInfExprCycle :: [Definition] -> [Expression] -> Either STypeError [InfExpr]
mkInfExprCycle defs exprs = execState (mkInfExprCycleS defs exprs) (InferEnv 1 (ReconcileEnv []) [])

mkInfExprCycleS :: [Definition] -> [Expression] -> State InferEnv (Either STypeError [InfExpr])
mkInfExprCycleS _ [] = return $ Right []
mkInfExprCycleS defs (expr : exprs) = do
  infTypE <- infFromExprS defs expr
  case infTypE of
    Left e -> return $ Left e
    Right (typ, _) -> do
      nextsE <- mkInfExprCycleS defs exprs
      case nextsE of
        Left e -> return $ Left e
        Right infexprs -> do
          typ' <- updateWithSubstitutionsI typ
          return $ Right $ InfExpr expr (mkNormType typ') : infexprs

infFromExprS :: [Definition] -> Expression -> State InferEnv (Either STypeError (Type, [Type]))
infFromExprS _ (Lit (Y.IntegerLiteral _)) = return $ Right (AtomicType AInt, [])
infFromExprS _ (Ident ident') = do
  (generics, lastGeneric) <- createGenericList ident'
  return $ Right (lastGeneric, generics)
infFromExprS defs (Ref refName) = do
  case lookupRefType defs refName of
    Nothing -> do
      globTyp <- getTypeOfGlobal refName
      return $ Right (globTyp, [])
    Just typ -> do
      shifted <- shiftNewIds typ
      return $ Right (shifted, [])
infFromExprS parts (Lambda _ expr) = do
  inferred <- infFromExprS parts expr
  case inferred of
    Left e -> return $ Left e
    Right (exprType, exprGenerics) -> do
      case exprGenerics of
        [] -> do
          paramId <- getNewId
          return $ Right (FunctionType (GenericType paramId) exprType, [])
        (paramType : otherGenerics) -> do
          return $ Right (FunctionType paramType exprType, otherGenerics)
infFromExprS parts (Application expr1 expr2) = do
  inferred1 <- infFromExprS parts expr1
  inferred2 <- infFromExprS parts expr2
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
            GenericType genericId -> trace (show expr1 ++ " " ++ show expr2 ++ " Gid: " ++ show genericId) $ do
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

lookupRefType :: [Definition] -> L.Ident -> Maybe NormType
lookupRefType parts refName =
  find
    ( \(Definition name _) -> name == refName
    )
    parts
    >>= \(Definition _ (InfExpr _ typ)) -> Just typ

mkProgramFromSyn :: Y.Program -> Either CompilerError Program
mkProgramFromSyn syn =
  first (SemanticError . STypeError) . mkProgram
    =<< first SemanticError . mkProgInfDeps
    =<< mkUninfProg syn

-- createSemanticProgram :: Y.Program -> Either CompilerError Program
-- createSemanticProgram prog = Program globs <$> (semProgParts >>= sinkError . checkSemProgParts declarations)
--   where
--     (ConvertEnv globs _ declarations _, semProgParts) =
--       runState (createSemanticPartsS prog) $
--         ConvertEnv [] [] [] []
--     sinkError :: Either STypeError a -> Either CompilerError a
--     sinkError (Left e) = Left $ SemanticError $ STypeError e
--     sinkError (Right a) = Right a
--
-- checkSemProgParts :: [(L.Ident, NormType)] -> [Definition] -> Either STypeError [Definition]
-- checkSemProgParts declarations =
--   mapM
--     ( \(Definition name infexpr) -> case lookup name declarations of
--         Nothing -> Right $ Definition name expr typ
--         Just declTyp -> case checkType declTyp typ of
--           Left e -> Left e
--           Right _ -> Right $ Definition name expr declTyp
--     )
--
-- parseAndCreateProgram :: SourceCode -> Either CompilerError Program
-- parseAndCreateProgram s = parseProgramSingleError s >>= createSemanticProgram
--
-- parseAndCreateExpressionWithProgram :: Program -> SourceCode -> Either CompilerError Expression
-- parseAndCreateExpressionWithProgram (Program _) s = do
--   parsed <- parseExpression s
--   return $ execState (createSemanticExpressionS parsed) (Env globs [] [] [])
--
-- lookupRefExp :: [Definition] -> L.Ident -> Maybe Expression
-- lookupRefExp parts refName =
--   find
--     ( \(Definition name _) -> name == refName
--     )
--     parts
--     >>= \(Definition _ expr) -> Just expr
--
--
-- mergeGlobalInfers ::
--   [(L.Ident, NormType)] ->
--   [(L.Ident, NormType)] ->
--   Either STypeError [(L.Ident, NormType)]
-- mergeGlobalInfers [] oldInfers = Right oldInfers
-- mergeGlobalInfers ((name, typ) : newInfers) oldInfers =
--   case lookup name oldInfers of
--     Nothing -> ((name, typ) :) <$> mergeGlobalInfers newInfers oldInfers
--     Just oldTyp -> do
--       let meTyps = mkMutExcTy2 oldTyp typ
--       mergedTyp <-
--         execState
--           ( reconcileTypesS
--               (ntType $ met2Fst meTyps)
--               (met2Snd meTyps)
--           )
--           (ReconcileEnv [])
--       ((name, mkNormType mergedTyp) :) <$> mergeGlobalInfers newInfers oldInfers
--
-- mergeWithCurrentGlobalInfers :: [(L.Ident, NormType)] -> State ConvertEnv (Either STypeError ())
-- mergeWithCurrentGlobalInfers newInfers = do
--   currEnv@(ConvertEnv _ _ _ globalInferences) <- get
--   let mergedE = mergeGlobalInfers newInfers globalInferences
--   case mergedE of
--     Left e -> return $ Left e
--     Right merged -> do
--       put $ currEnv {globalInfers = merged}
--       return $ Right ()
--
-- emptyProgram :: Program
-- emptyProgram = Program [] []
--
-- mkDepInfExpr :: [Definition] -> Expression -> Either STypeError InfExpr
-- mkDepInfExpr parts expr = (,normRefs) <$> normTypeE
--   where
--     (ienv, typE) = runState (mkDepInfExprS parts expr) (InferEnv 1 (ReconcileEnv []) [])
--     normTypeE = mkNormType . fst <$> typE
--     refs = execState getAllTypesInferredToRefsSI ienv
--     normRefs = second mkNormType <$> refs
--
-- mkInfExpr :: [Definition] -> Expression -> Either STypeError InfExpr
-- mkInfExpr parts expr = do
--   (normType, refs) <- mkDepInfExpr parts expr
--   return $ InfExpr expr normType refs
--
-- -- Unit tests
--
-- test :: SourceCode -> Either CompilerError Expression
-- test s = convertExpression <$> Y.parseExpression s
--
-- testT :: SourceCode -> Either CompilerError NormType
-- testT s = convertType <$> Y.parseType s
--
-- testP :: SourceCode -> Either CompilerError Program
-- testP s = Y.parseProgramSingleError s >>= createSemanticProgram
--
-- -- Unit tests
--
-- nai :: Type
-- nai = AtomicType AInt
--
-- lam :: String -> Expression -> Expression
-- lam s = Lambda (L.Ident s)
--
-- funt :: Type -> Type -> Type
-- funt = FunctionType
--
-- mkn :: Type -> NormType
-- mkn = mkNormType
--
-- ident :: String -> L.Ident
-- ident = L.Ident
--
-- defi :: String -> Expression -> Definition
-- defi s = Definition (ident s)
--
-- semanticAnalyzerTests :: [Bool]
-- semanticAnalyzerTests =
--   [ test "\\a.a" == Right (lam "a" (Ident 1)),
--     test "\\a.\\b.a b" == Right (lam "a" (lam "b" (Application (Ident 2) (Ident 1)))),
--     test "(\\a.a) x" == Left (SemanticError $ SUndefinedVariable "x"),
--     testT "int" == Right (mkn nai),
--     testT "int -> int" == Right (mkn $ funt nai nai),
--     testT "(int -> int) -> int" == Right (mkn $ funt (funt nai nai) nai),
--     testP program1 == Right program1ShouldBe,
--     testP program2 == Right program2ShouldBe,
--     testP program3 == Left (SemanticError $ SUndefinedVariable "notE"),
--     parseAndCreateProgram program1 == Right program1ShouldBe,
--     parseAndCreateProgram program2 == Right program2ShouldBe,
--     (parseAndCreateProgram program1 >>= (`parseAndCreateExpressionWithProgram` "true")) == Right (Ref $ ident "true"),
--     parseAndCreateProgram program4 == Right program4ShouldBe,
--     parseAndCreateProgram program5 == Right program5ShouldBe,
--     parseAndCreateProgram program6 == Right program6ShouldBe
--   ]
--
-- program1 :: SourceCode
-- program1 =
--   "true := \\a.\\b.a\n"
--     ++ "false := \\a.\\b.b"
--
-- program1ShouldBe :: Program
-- program1ShouldBe =
--   Program
--     [ident "true", ident "false"]
--     [ Definition "true" (lam "a" (lam "b" (Ident 2))) (mkn $ funt (GenericType 1) (funt (GenericType 2) (GenericType 1))),
--       Definition "false" (lam "a" (lam "b" (Ident 1))) (mkn $ funt (GenericType 1) (funt (GenericType 2) (GenericType 2)))
--     ]
--
-- program2 :: SourceCode
-- program2 =
--   "true := \\a.\\b.a\n"
--     ++ "false := \\a.\\b.b\n"
--     ++ "and := \\a.\\b. a b false\n"
--     ++ "or := \\a.\\b. a true b\n"
--     ++ "not := \\a. a false true\n"
--     ++ "xor := \\a.\\b. a (not b) b"
--
-- program2ShouldBe :: Program
-- program2ShouldBe =
--   Program
--     [ident "true", ident "false", ident "and", ident "or", ident "not", ident "xor"]
--     [ Definition (ident "true") (lam "a" (lam "b" (Ident 2))) (funt (GenericType 1) (funt (GenericType 2) (GenericType 1))),
--       Definition "false" (lam "a" (lam "b" (Ident 1))) (funt (GenericType 1) (funt (GenericType 2) (GenericType 2))),
--       Definition "and" (lam "a" (lam "b" (Application (Application (Ident 2) (Ident 1)) (Ref "false")))) (funt (funt (GenericType 1) (funt (funt (GenericType 2) (funt (GenericType 3) (GenericType 3))) (GenericType 4))) (funt (GenericType 1) (GenericType 4))),
--       Definition "or" (lam "a" (lam "b" (Application (Application (Ident 2) (Ref "true")) (Ident 1)))) (funt (funt (funt (GenericType 1) (funt (GenericType 2) (GenericType 1))) (funt (GenericType 3) (GenericType 4))) (funt (GenericType 3) (GenericType 4))),
--       Definition "not" (lam "a" (Application (Application (Ident 1) (Ref "false")) (Ref "true"))) (funt (funt (funt (GenericType 1) (funt (GenericType 2) (GenericType 2))) (funt (funt (GenericType 3) (funt (GenericType 4) (GenericType 3))) (GenericType 5))) (GenericType 5)),
--       Definition "xor" (lam "a" (lam "b" (Application (Application (Ident 2) (Application (Ref "not") (Ident 1))) (Ident 1)))) (funt (funt (GenericType 1) (funt (funt (funt (GenericType 2) (funt (GenericType 3) (GenericType 3))) (funt (funt (GenericType 4) (funt (GenericType 5) (GenericType 4))) (GenericType 1))) (GenericType 6))) (funt (funt (funt (GenericType 2) (funt (GenericType 3) (GenericType 3))) (funt (funt (GenericType 4) (funt (GenericType 5) (GenericType 4))) (GenericType 1))) (GenericType 6)))
--     ]
--
-- program3 :: SourceCode
-- program3 =
--   "true := \\a.\\b.a\n"
--     ++ "false := \\a.\\b.b\n"
--     ++ "and := \\a.\\b. a b false\n"
--     ++ "or := \\a.\\b. a true b\n"
--     ++ "not := \\a. a false true\n"
--     ++ "xor := \\a.\\b. a (notE b) b"
--
-- program4 :: SourceCode
-- program4 =
--   "te := \\a. \\b. a b\n"
--
-- program4ShouldBe :: Program
-- program4ShouldBe =
--   Program
--     ["te"]
--     [Definition "te" (lam "a" (lam "b" (Application (Ident 2) (Ident 1)))) (funt (funt (GenericType 1) (GenericType 2)) (funt (GenericType 1) (GenericType 2)))]
--
-- program5 :: SourceCode
-- program5 =
--   "zero := \\f. \\z. z\n"
--     ++ "succ := \\n. \\f. \\z. f (n f z)\n"
--     ++ "one := succ zero\n"
--     ++ "two := succ one\n"
--     ++ "three := succ two\n"
--
-- program5ShouldBe :: Program
-- program5ShouldBe =
--   Program
--     ["zero", "succ", "one", "two", "three"]
--     [ Definition "zero" (lam "f" (lam "z" (Ident 1))) (funt (GenericType 1) (funt (GenericType 2) (GenericType 2))),
--       Definition "succ" (lam "n" (lam "f" (lam "z" (Application (Ident 2) (Application (Application (Ident 3) (Ident 2)) (Ident 1)))))) (funt (funt (funt (GenericType 1) (GenericType 2)) (funt (GenericType 3) (GenericType 1))) (funt (funt (GenericType 1) (GenericType 2)) (funt (GenericType 3) (GenericType 2)))),
--       Definition "one" (Application (Ref "succ") (Ref "zero")) (funt (funt (GenericType 1) (GenericType 2)) (funt (GenericType 1) (GenericType 2))),
--       Definition "two" (Application (Ref "succ") (Ref "one")) (funt (funt (GenericType 1) (GenericType 1)) (funt (GenericType 1) (GenericType 1))),
--       Definition "three" (Application (Ref "succ") (Ref "two")) (funt (funt (GenericType 1) (GenericType 1)) (funt (GenericType 1) (GenericType 1)))
--     ]
--
-- program6ShouldBe :: Program
-- program6ShouldBe =
--   Program
--     ["true", "false"]
--     [ Definition "true" (lam "a" (lam "b" (Ident 2))) (funt (GenericType 1) (funt (GenericType 1) (GenericType 1))),
--       Definition "false" (lam "a" (lam "b" (Ident 1))) (funt (GenericType 1) (funt (GenericType 1) (GenericType 1)))
--     ]
--
-- program6 :: SourceCode
-- program6 =
--   "true : a -> a -> a\n"
--     ++ "true := \\a.\\b.a\n"
--     ++ "false : a -> a -> a\n"
--     ++ "false := \\a.\\b.b\n"
--
-- at = \x -> bt (x + 1 :: Int)
--
-- bt = \x -> at x