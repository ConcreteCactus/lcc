{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wincomplete-patterns #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module SemanticAnalyzer.Internal where

import Control.Applicative
import Control.Monad
import Data.Bifunctor
import Data.Foldable
import Errors
import qualified Lexer as L
import SemanticAnalyzer.DependencyList
import SemanticAnalyzer.Expression
import SemanticAnalyzer.Type
import StandardLibrary
import qualified SyntacticAnalyzer as Y
import Util

stdLib ::
  [ ( L.VarIdent
    , NormType
    , (Int -> Writer () String) ->
      Writer () [String]
    )
  ]
stdLib = standardLibrary

data UninfDefinition = UninfDefinition
  { udefName :: L.VarIdent
  , udefPos :: TextPos
  , udefExpr :: Expression
  , udefWish :: Maybe NormType
  }
  deriving (Show)

data InfExpr = InfExpr
  { ieExpr :: Expression
  , ieType :: NormType
  }
  deriving (Eq)

instance Show InfExpr where
  show expr = show (ieExpr expr) ++ " : " ++ show (ieType expr)

data TypedExpr
  = InfTyExpr InfExpr
  | WishTyExpr Expression NormType
  deriving (Eq)

instance Show TypedExpr where
  show (InfTyExpr infExpr) = show infExpr
  show (WishTyExpr expr typ) = show expr ++ " : " ++ show typ

data Definition = Definition
  { defName :: L.VarIdent
  , defExpr :: TypedExpr
  }
  deriving (Eq)

instance Show Definition where
  show (Definition name expr) = show name ++ " := " ++ show expr

newtype UninfProg
  = UninfProg [UninfDefinition]
  deriving (Show)

data ProgInfDeps = ProgInfDeps
  { pidUninfProg :: UninfProg
  , pidDepGraph :: DependencyList L.VarIdent
  }

newtype Program = Program
  { progDefs :: [Definition]
  }
  deriving (Eq, Show)

type SourceCode = String

teExpr :: TypedExpr -> Expression
teExpr (InfTyExpr infExpr) = ieExpr infExpr
teExpr (WishTyExpr expr _) = expr

teType :: TypedExpr -> NormType
teType (InfTyExpr infExpr) = ieType infExpr
teType (WishTyExpr _ typ) = typ

stConvertEnv :: ConvertEnv
stConvertEnv =
  ConvertEnv
    { ceScope = []
    , ceGlobals = map (\(a, _, _) -> a) stdLib
    , ceDecls = map (\(a, b, _) -> (a, b)) stdLib
    }

mkUninfProg :: Y.Program -> Either SemanticError UninfProg
mkUninfProg synProg = execState (mkUninfProgS synProg) stConvertEnv

mkUninfProgS :: Y.Program -> State ConvertEnv (Either SemanticError UninfProg)
mkUninfProgS synProg = do
  udefsE <- foldEitherM (flip helper) [] synProg
  case udefsE of
    Left e -> return $ Left e
    Right udefs -> do
      errors <-
        mapM
          (\def -> checkUndefinedReferencesS (udefPos def) (udefExpr def))
          udefs
      udefs' <- mapM addWish udefs
      case find
        ( \case
            Just _ -> True
            _ -> False
        )
        errors of
        Just (Just e) -> return $ Left e
        Just Nothing ->
          error
            $ "The computer has been struck by a lightning bolt.\n"
            ++ "Please move away from the computer as fast as possible."
        Nothing -> return $ Right $ UninfProg udefs'
 where
  foldEitherM ::
    (Monad m) => (a -> b -> m (Either e b)) -> b -> [a] -> m (Either e b)
  foldEitherM _ acc [] = return $ Right acc
  foldEitherM f acc (x : xs) = do
    res <- f x acc
    case res of
      Left err -> return $ Left err
      Right newAcc -> foldEitherM f newAcc xs
  helper ::
    [UninfDefinition] ->
    Y.ProgramPart ->
    State ConvertEnv (Either SemanticError [UninfDefinition])
  helper parts (Y.Declaration _ name typ) = do
    let conTyp = convertType typ
    addDecl name conTyp
    return $ Right parts
  helper parts (Y.Definition pos name expr) = do
    semExpr <- convertExpressionS expr
    addGlobal name
    return $ Right $ parts ++ [UninfDefinition name pos semExpr Nothing]
  checkUndefinedReferencesS ::
    TextPos -> Expression -> State ConvertEnv (Maybe SemanticError)
  checkUndefinedReferencesS _ (Ident _) = return Nothing
  checkUndefinedReferencesS _ (Lit _) = return Nothing
  checkUndefinedReferencesS pos (Ref name) = do
    globM <- findGlobal name
    case globM of
      Nothing ->
        return $ Just $ mkSemErr pos $ SeUndefinedVariable (show name)
      Just _ -> return Nothing
  checkUndefinedReferencesS pos (Lambda _ expr) =
    checkUndefinedReferencesS pos expr
  checkUndefinedReferencesS pos (Application expr1 expr2) = do
    c1 <- checkUndefinedReferencesS pos expr1
    c2 <- checkUndefinedReferencesS pos expr2
    case (c1, c2) of
      (Just e, _) -> return $ Just e
      (_, Just e) -> return $ Just e
      _ -> return Nothing
  checkUndefinedReferencesS pos (IfThenElse cond expr1 expr2) = do
    cond' <- checkUndefinedReferencesS pos cond
    expr1' <- checkUndefinedReferencesS pos expr1
    expr2' <- checkUndefinedReferencesS pos expr2
    case (cond', expr1', expr2') of
      (Just e, _, _) -> return $ Just e
      (_, Just e, _) -> return $ Just e
      (_, _, Just e) -> return $ Just e
      _ -> return Nothing
  addWish :: UninfDefinition -> State ConvertEnv UninfDefinition
  addWish udef = do
    (ConvertEnv _ _ wishes) <- get
    case lookup (udefName udef) wishes of
      Nothing -> return udef
      Just wish -> return udef{udefWish = Just wish}

-- todo: Make it possible to return multiple errors
mkProgInfDeps :: UninfProg -> Either SemanticError ProgInfDeps
mkProgInfDeps uiprog@(UninfProg uiDefs) =
  case checkDeps uiprog of
    Just e -> Left e
    Nothing ->
      Right
        $ ProgInfDeps uiprog
        $ mkDependencyList (map udefName uiDefs) (getDeps uiprog)
 where
  checkDeps :: UninfProg -> Maybe SemanticError
  checkDeps (UninfProg uiDefs') =
    foldr
      ( \udef acc ->
          ( case find
              ( \d ->
                  d
                    `notElem` map udefName uiDefs'
                    && d
                    `notElem` map (\(a, _, _) -> a) stdLib
              )
              (getAllRefs (udefExpr udef)) of
              Just e ->
                Just
                  $ mkSemErr (udefPos udef)
                  $ SeUndefinedVariable
                  $ show e
              Nothing -> acc
          )
      )
      Nothing
      uiDefs'
  getDeps :: UninfProg -> L.VarIdent -> [L.VarIdent]
  getDeps (UninfProg uiDefs') glob =
    case find ((== glob) . udefName) uiDefs' of
      Nothing -> error "Found an undefined global. This is a bug."
      Just definition -> getAllRefs $ udefExpr definition
  getAllRefs :: Expression -> [L.VarIdent]
  getAllRefs (Ident _) = []
  getAllRefs (Lit _) = []
  getAllRefs (Ref name) = [name]
  getAllRefs (Lambda _ expr) = getAllRefs expr
  getAllRefs (Application expr1 expr2) = getAllRefs expr1 +-+ getAllRefs expr2
  getAllRefs (IfThenElse cond expr1 expr2) =
    getAllRefs cond +-+ getAllRefs expr1 +-+ getAllRefs expr2

mkProgram :: ProgInfDeps -> Either TypeError Program
mkProgram (ProgInfDeps uprog (DependencyList dList)) =
  Program
    <$> foldl
      ( \acc item ->
          case item of
            DepListSingle a -> helperTree uprog a acc
            DepListCycle as -> helperCycle uprog as acc
      )
      (Right [])
      dList
 where
  helperTree ::
    UninfProg ->
    L.VarIdent ->
    Either TypeError [Definition] ->
    Either TypeError [Definition]
  helperTree _ _ (Left e) = Left e
  helperTree uprog' a (Right prevDeps) =
    (: prevDeps)
      <$> (Definition a <$> mkInfExprTree prevDeps (lookupUDefUnsafe uprog' a))
  helperCycle ::
    UninfProg ->
    [L.VarIdent] ->
    Either TypeError [Definition] ->
    Either TypeError [Definition]
  helperCycle _ _ (Left e) = Left e
  helperCycle uprog' as (Right prevDeps) =
    (++ prevDeps)
      <$> ( zipWith Definition as
              <$> mkTyExprCycle prevDeps (lookupUDefUnsafe uprog' <$> as)
          )
  lookupUDefUnsafe :: UninfProg -> L.VarIdent -> UninfDefinition
  lookupUDefUnsafe (UninfProg udefs) sname =
    case find (\(UninfDefinition name _ _ _) -> name == sname) udefs of
      Nothing -> error "A global definition wasn't found."
      Just a -> a

mkInfExprTree :: [Definition] -> UninfDefinition -> Either TypeError TypedExpr
mkInfExprTree defs udef =
  execState (mkTyExprTreeS defs udef) (InferEnv 1 (ReconcileEnv []) [])

mkTyExprTreeS ::
  [Definition] ->
  UninfDefinition ->
  State InferEnv (Either TypeError TypedExpr)
mkTyExprTreeS defs udef = do
  itypE <- infFromExprS defs (udefExpr udef)
  case itypE of
    Left e -> return $ Left $ mkTypErr (udefPos udef) e
    Right (ityp, _) -> do
      InferEnv _ _ globs <- get
      case lookup (udefName udef) globs of
        Nothing -> do
          let infExpr = InfExpr (udefExpr udef) $ mkNormType ityp
          return $ addWish udef infExpr
        Just _ -> do
          defTyp <- getTypeOfGlobal (udefName udef)
          mergedDefTypE <- reconcileTypesIS ityp defTyp
          case mergedDefTypE of
            Left e -> return $ Left $ mkTypErr (udefPos udef) e
            Right mergedDefTyp -> do
              let infExpr = InfExpr (udefExpr udef) (mkNormType mergedDefTyp)
              return $ addWish udef infExpr
 where
  addWish :: UninfDefinition -> InfExpr -> Either TypeError TypedExpr
  addWish udef' infExpr@(InfExpr _ _) =
    case udefWish udef' of
      Nothing -> Right $ mkTypedExprInf infExpr
      Just wish ->
        leftMap (mkTypErr (udefPos udef'))
          $ mkTypedExprWish infExpr wish

mkTyExprCycle ::
  [Definition] -> [UninfDefinition] -> Either TypeError [TypedExpr]
mkTyExprCycle defs udefs =
  execState (mkTyExprCycleS defs udefs) (InferEnv 1 (ReconcileEnv []) [])

mkTyExprCycleS ::
  [Definition] ->
  [UninfDefinition] ->
  State InferEnv (Either TypeError [TypedExpr])
mkTyExprCycleS defs udefs = do
  defTypDictE <- inferTypeExprCycleS defs udefs
  case defTypDictE of
    Left e -> return $ Left e
    Right defTypDict -> do
      helpedTypesE <- sequence <$> mapM helper defTypDict
      case helpedTypesE of
        Left e -> return $ Left e
        Right helpedTypes -> do
          updatedTypes <-
            mapM
              (\(ty, isW) -> (,isW) <$> updateWithSubstitutionsI ty)
              helpedTypes
          return $ zipWithM (curry makeIntoTyExpr) udefs updatedTypes
 where
  helper ::
    (UninfDefinition, Type) ->
    State InferEnv (Either TypeError (Type, Bool))
  helper (udef, infTyp) = do
    infTyp' <- updateWithSubstitutionsI infTyp
    case udefWish udef of
      Nothing -> return $ Right (infTyp', False)
      Just wTyp -> do
        case checkType (mkMutExcTy2 wTyp (mkNormType infTyp')) of
          Left e -> return $ Left $ mkTypErr (udefPos udef) e
          Right _ -> do
            wTyp' <- shiftNewIds wTyp
            recTypE <- reconcileTypesIS wTyp' infTyp'
            case recTypE of
              Left e -> error $ "Reconcile after check failed" ++ show e
              Right _ -> return $ Right (wTyp', True)
  makeIntoTyExpr ::
    (UninfDefinition, (Type, Bool)) -> Either TypeError TypedExpr
  makeIntoTyExpr (udef, (typ, isWish)) =
    if isWish
      then Right $ mkTypedExprInf (InfExpr (udefExpr udef) (mkNormType typ))
      else
        leftMap (mkTypErr (udefPos udef))
          $ mkTypedExprWish
            (InfExpr (udefExpr udef) (mkNormType typ))
            (mkNormType typ)

inferTypeExprCycleS ::
  [Definition] ->
  [UninfDefinition] ->
  State InferEnv (Either TypeError [(UninfDefinition, Type)])
inferTypeExprCycleS _ [] = return $ Right []
inferTypeExprCycleS defs (udef : udefs) = do
  infTypE <- infFromExprS defs (udefExpr udef)
  case infTypE of
    Left e -> return $ Left $ mkTypErr (udefPos udef) e
    Right (ityp, _) -> do
      selfTyp <- getTypeOfGlobal (udefName udef)
      selfRecE <- reconcileTypesIS ityp selfTyp
      case selfRecE of
        Left e -> return $ Left $ mkTypErr (udefPos udef) e
        Right ityp' -> do
          nextsE <- inferTypeExprCycleS defs udefs
          case nextsE of
            Left e -> return $ Left e
            Right nexts -> do
              ityp'' <- updateWithSubstitutionsI ityp'
              return $ Right $ (udef, ityp'') : nexts

infFromExprS ::
  [Definition] ->
  Expression ->
  State InferEnv (Either TypeErrorType (Type, [Type]))
infFromExprS _ (Lit (Y.Literal typ _)) =
  return $ Right (AtomicType typ, [])
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
      newGenericsM <-
        sequence
          <$> forgivingZipWithME reconcileTypesIS expr1Generics expr2Generics
      case newGenericsM of
        Left e -> return $ Left e
        Right newGenerics -> do
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
                  updatedGenerics <- mapM updateWithSubstitutionsI newGenerics
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
                  updatedGenerics <- mapM updateWithSubstitutionsI newGenerics
                  return $ Right (GenericType newReturnId, updatedGenerics)
            typ -> return $ Left $ TeApplyingToANonFunction $ show typ
infFromExprS parts (IfThenElse cond expr1 expr2) = do
  inferredCond <- infFromExprS parts cond
  inferred1 <- infFromExprS parts expr1
  inferred2 <- infFromExprS parts expr2
  case (inferredCond, inferred1, inferred2) of
    (Left e, _, _) -> return $ Left e
    (_, Left e, _) -> return $ Left e
    (_, _, Left e) -> return $ Left e
    ( Right (condType, condGenerics)
      , Right (expr1Type, expr1Generics)
      , Right (expr2Type, expr2Generics)
      ) -> do
        genericsE <- do
          newGenerics1E <-
            forgivingZipWithMM
              reconcileTypesIS
              condGenerics
              expr1Generics
          case newGenerics1E of
            Left e -> return $ Left e
            Right newGenerics1 ->
              forgivingZipWithMM
                reconcileTypesIS
                expr2Generics
                newGenerics1
        updatedCondType <- updateWithSubstitutionsI condType
        updatedExpr1Type <- updateWithSubstitutionsI expr1Type
        updatedExpr2Type <- updateWithSubstitutionsI expr2Type
        condWorked <- case updatedCondType of
          GenericType genericId ->
            addNewSubstitutionI genericId $ AtomicType Y.ABool
          AtomicType Y.ABool -> return $ Right ()
          _ -> return $ Left TeIfThenElseConditionIsNotBool
        reconciledE <- reconcileTypesIS updatedExpr1Type updatedExpr2Type
        case (genericsE, condWorked, reconciledE) of
          (Left e, _, _) -> return $ Left e
          (_, Left e, _) -> return $ Left e
          (_, _, Left e) -> return $ Left e
          (Right generics, Right (), Right reconciled) ->
            return $ Right (reconciled, generics)

lookupRefType :: [Definition] -> L.VarIdent -> Maybe NormType
lookupRefType parts refName =
  ( find (\(Definition name _) -> name == refName) parts
      >>= ( \case
              (Definition _ (WishTyExpr _ wtyp)) -> Just wtyp
              (Definition _ (InfTyExpr infExpr)) -> Just (ieType infExpr)
          )
  )
    <|> (\(_, a, _) -> a)
    <$> find (\(name, _, _) -> name == refName) stdLib

mkProgramFromSyn :: Y.Program -> Either CompilerError Program
mkProgramFromSyn syn =
  first mkCompErrTyp
    . mkProgram
    =<< first mkCompErrSem
    . mkProgInfDeps
    =<< leftMap mkCompErrSem (mkUninfProg syn)

mkTypedExprInf :: InfExpr -> TypedExpr
mkTypedExprInf = InfTyExpr

mkTypedExprWish :: InfExpr -> NormType -> Either TypeErrorType TypedExpr
mkTypedExprWish infExpr typ =
  WishTyExpr (ieExpr infExpr) typ
    <$ checkType (mkMutExcTy2 typ (ieType infExpr))
