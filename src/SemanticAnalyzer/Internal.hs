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
    udefExpr :: Expression,
    udefWish :: Maybe NormType
  }
  deriving (Show)

data InfExpr = InfExpr
  { ieExpr :: Expression,
    ieType :: NormType
  }
  deriving (Eq)

instance Show InfExpr where
  show expr = show (ieExpr expr) ++ " : " ++ show (ieType expr)

data TypedExpr = InfTyExpr InfExpr | WishTyExpr InfExpr NormType
  deriving (Eq)

instance Show TypedExpr where
  show (InfTyExpr infExpr) = show infExpr
  show (WishTyExpr infExpr typ) = show (ieExpr infExpr) ++ " : " ++ show typ

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
teExpr (InfTyExpr infExpr) = ieExpr infExpr
teExpr (WishTyExpr infExpr _) = ieExpr infExpr

teType :: TypedExpr -> NormType
teType (InfTyExpr infExpr) = ieType infExpr
teType (WishTyExpr _ typ) = typ

mkUninfProg :: Y.Program -> Either CompilerError UninfProg
mkUninfProg synProg = execState (mkUninfProgS synProg) (ConvertEnv [] [] [])

mkUninfProgS :: Y.Program -> State ConvertEnv (Either CompilerError UninfProg)
mkUninfProgS synProg = do
  udefsE <- foldEitherM (flip helper) [] synProg
  case udefsE of
    Left e -> return $ Left e
    Right udefs -> do
      errors <- mapM (checkUndefinedReferencesS . udefExpr) udefs
      udefs' <- mapM addWish udefs
      case find
        ( \case
            Just _ -> True
            _ -> False
        )
        errors of
        Just (Just e) -> return $ Left e
        Just Nothing -> error "I've got hit by a lightning bolt."
        Nothing -> return $ Right $ UninfProg udefs'
  where
    foldEitherM ::
      (Monad m) =>
      (a -> b -> m (Either e b)) ->
      b ->
      [a] ->
      m (Either e b)
    foldEitherM _ acc [] = return $ Right acc
    foldEitherM f acc (x : xs) = do
      res <- f x acc
      case res of
        Left err -> return $ Left err
        Right newAcc -> foldEitherM f newAcc xs
    helper ::
      [UninfDefinition] ->
      Y.ProgramPart ->
      State ConvertEnv (Either CompilerError [UninfDefinition])
    helper parts (Y.Declaration name typ) = do
      let conTyp = convertType typ
      addDecl name conTyp
      return $ Right parts
    helper parts (Y.Definition name expr) = do
      semExpr <- convertExpressionS expr
      addGlobal name
      return $ Right $ parts ++ [UninfDefinition name semExpr Nothing]
    checkUndefinedReferencesS ::
      Expression -> State ConvertEnv (Maybe CompilerError)
    checkUndefinedReferencesS (Ident _) = return Nothing
    checkUndefinedReferencesS (Lit _) = return Nothing
    checkUndefinedReferencesS (Ref name) = do
      globM <- findGlobal name
      case globM of
        Nothing ->
          return $ Just $ SemanticError $ SUndefinedVariable (L.unIdent name)
        Just _ -> return Nothing
    checkUndefinedReferencesS (Lambda _ expr) = checkUndefinedReferencesS expr
    checkUndefinedReferencesS (Application expr1 expr2) = do
      c1 <- checkUndefinedReferencesS expr1
      c2 <- checkUndefinedReferencesS expr2
      case (c1, c2) of
        (Just e, _) -> return $ Just e
        (_, Just e) -> return $ Just e
        _ -> return Nothing
    addWish :: UninfDefinition -> State ConvertEnv UninfDefinition
    addWish udef = do
      (ConvertEnv _ _ wishes) <- get
      case lookup (udefName udef) wishes of
        Nothing -> return udef
        Just wish -> return udef {udefWish = Just wish}

mkProgInfDeps :: UninfProg -> Either SemanticError ProgInfDeps
mkProgInfDeps uiprog@(UninfProg uiDefs) = case checkDeps uiprog of
  Just e -> Left e
  Nothing ->
    Right $
      ProgInfDeps uiprog $
        mkDependencyGraph (map udefName uiDefs) (getDeps uiprog)
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
    lookupUDefUnsafe :: UninfProg -> L.Ident -> UninfDefinition
    lookupUDefUnsafe (UninfProg udefs) sname = case find (\(UninfDefinition name _ _) -> name == sname) udefs of
      Nothing -> error "A global definition wasn't found."
      Just a -> a

mkInfExprTree :: [Definition] -> UninfDefinition -> Either STypeError InfExpr
mkInfExprTree defs udef = execState (mkInfExprTreeS defs udef) (InferEnv 1 (ReconcileEnv []) [])

mkInfExprTreeS :: [Definition] -> UninfDefinition -> State InferEnv (Either STypeError InfExpr)
mkInfExprTreeS defs udef = do
  itypE <- infFromExprS defs (udefExpr udef)
  case itypE of
    Left e -> return $ Left e
    Right (ityp, _) -> do
      InferEnv _ _ globs <- get
      case lookup (udefName udef) globs of
        Nothing -> return $ Right $ InfExpr (udefExpr udef) $ mkNormType ityp
        Just _ -> do
          defTyp <- getTypeOfGlobal (udefName udef)
          mergedDefTypE <- reconcileTypesIS ityp defTyp
          return $ InfExpr (udefExpr udef) . mkNormType <$> mergedDefTypE

mkInfExprCycle :: [Definition] -> [UninfDefinition] -> Either STypeError [InfExpr]
mkInfExprCycle defs udefs = execState (mkInfExprCycleS defs udefs) (InferEnv 1 (ReconcileEnv []) [])

mkInfExprCycleS :: [Definition] -> [UninfDefinition] -> State InferEnv (Either STypeError [InfExpr])
mkInfExprCycleS _ [] = return $ Right []
mkInfExprCycleS defs (udef : udefs) = do
  globTyp <- getTypeOfGlobal (udefName udef)
  infTypE <- infFromExprS defs (udefExpr udef)
  case infTypE of
    Left e -> return $ Left e
    Right (ityp, _) -> do
      _ <- reconcileTypesIS globTyp ityp
      nextsE <- mkInfExprCycleS defs udefs
      case nextsE of
        Left e -> return $ Left e
        Right infexprs -> do
          ityp' <- updateWithSubstitutionsI ityp
          InferEnv _ _ globs <- get
          case lookup (udefName udef) globs of
            Nothing -> return $ Right $ InfExpr (udefExpr udef) (mkNormType ityp') : infexprs
            Just _ -> do
              defTyp <- getTypeOfGlobal (udefName udef)
              mergedDefTypE <- reconcileTypesIS ityp defTyp
              case mergedDefTypE of
                Left e -> return $ Left e
                Right mergedDefTyp ->
                  return $
                    Right $
                      InfExpr (udefExpr udef) (mkNormType mergedDefTyp) : infexprs

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

mkTypedExprInf :: InfExpr -> TypedExpr
mkTypedExprInf = InfTyExpr

mkTypedExprWish :: InfExpr -> NormType -> Either STypeError NormType
mkTypedExprWish infExpr typ = typ <$ checkType (mkMutExcTy2 (ieType infExpr) typ)
