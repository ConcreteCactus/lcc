{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module SemanticAnalyzer.Type.Internal where

import Control.Monad
import Data.Bifunctor
import Errors
import qualified Lexer as L
import qualified SyntacticAnalyzer as Y
import Util

-- Improvement idea: implement foldable for type

data Type
  = AtomicType Y.AtomicType
  | GenericType Int
  | FunctionType Type Type
  | SumType Type Type
  | ProductType Type Type
  | ListType Type
  deriving (Eq)

data NormType = NormType
  { ntType :: Type
  , ntMaxId :: Int
  }
  deriving (Eq)

instance Show Type where
  show (AtomicType a) = show a
  show (GenericType n) = "T" ++ show n
  show (FunctionType t1@(FunctionType _ _) t2) = "(" ++ show t1 ++ ") -> " ++ show t2
  show (FunctionType t1 t2) = show t1 ++ " -> " ++ show t2
  show (SumType t1@(SumType _ _) t2) = "(" ++ show t1 ++ ") + " ++ show t2
  show (SumType t1 t2) = show t1 ++ " + " ++ show t2
  show (ProductType t1@(ProductType _ _) t2) = "(" ++ show t1 ++ ") * " ++ show t2
  show (ProductType t1 t2) = show t1 ++ " * " ++ show t2
  show (ListType typ) = "[" ++ show typ ++ "]"

instance Show NormType where
  show nt = show $ ntType nt

-- Mutually exclusive types (they don't share generic ids)
data MutExcTy2 = MutExcTy2
  { met2Fst :: NormType
  , met2Snd :: Type
  }
  deriving (Show)

data InferEnv = InferEnv Int ReconcileEnv [(L.VarIdent, Int)] deriving (Show)

-- Laws
--   1. forall (a, _) in RE : forall (_, b) in RE : forall genericId in b : a != genericId
--   2. forall (a, b), (a', b') in RE : b != b' => a != a'
newtype ReconcileEnv = ReconcileEnv [(Int, Type)] deriving (Show)

data ConvertEnv a = ConvertEnv
  { ceNextId :: Int
  , ceDict :: [(a, Int)]
  }

convertType :: Y.Type -> NormType
convertType synType = NormType typ (nextId - 1)
 where
  (ConvertEnv nextId _, typ) =
    runState (convertTypeS synType) (ConvertEnv 1 [])

convertTypeS :: Y.Type -> State (ConvertEnv L.TypIdent) Type
convertTypeS (Y.TypeName t) = return $ AtomicType t
convertTypeS (Y.TypeId id') = do
  ConvertEnv ident decls <- get
  case lookup id' decls of
    Just ident' -> return $ GenericType ident'
    Nothing -> do
      put $ ConvertEnv (ident + 1) ((id', ident) : decls)
      return $ GenericType ident
convertTypeS (Y.FunctionType t1 t2) = do
  t1' <- convertTypeS t1
  t2' <- convertTypeS t2
  return $ FunctionType t1' t2'
convertTypeS (Y.SumType t1 t2) = do
  t1' <- convertTypeS t1
  t2' <- convertTypeS t2
  return $ SumType t1' t2'
convertTypeS (Y.ProductType t1 t2) = do
  t1' <- convertTypeS t1
  t2' <- convertTypeS t2
  return $ ProductType t1' t2'
convertTypeS (Y.ListType typ) = do
  typ' <- convertTypeS typ
  return $ ListType typ'

mkNormType :: Type -> NormType
mkNormType typ = NormType typ' (maxId - 1)
 where
  (ConvertEnv maxId _, typ') = runState (mkNormTypeS typ) (ConvertEnv 1 [])

mkNormTypeS :: Type -> State (ConvertEnv Int) Type
mkNormTypeS (GenericType ind) = do
  env <- get
  case lookup ind (ceDict env) of
    Nothing -> do
      put
        $ ConvertEnv
          { ceNextId = ceNextId env + 1
          , ceDict = (ind, ceNextId env) : ceDict env
          }
      return $ GenericType (ceNextId env)
    Just ind' -> return $ GenericType ind'
mkNormTypeS (AtomicType atomicType) = return $ AtomicType atomicType
mkNormTypeS (FunctionType paramType returnType) = do
  normalizedParam <- mkNormTypeS paramType
  normalizedReturn <- mkNormTypeS returnType
  return $ FunctionType normalizedParam normalizedReturn
mkNormTypeS (SumType t1 t2) = do
  nt1 <- mkNormTypeS t1
  nt2 <- mkNormTypeS t2
  return $ SumType nt1 nt2
mkNormTypeS (ProductType t1 t2) = do
  nt1 <- mkNormTypeS t1
  nt2 <- mkNormTypeS t2
  return $ ProductType nt1 nt2
mkNormTypeS (ListType typ) = do
  typ' <- mkNormTypeS typ
  return $ ListType typ'

mkMutExcTy2 :: NormType -> NormType -> MutExcTy2
mkMutExcTy2 original shiftCandidate =
  MutExcTy2 original
    $ fst
    $ shiftIds (ntMaxId original + 1)
    $ ntType shiftCandidate

shiftNewIds :: NormType -> State InferEnv Type
shiftNewIds typ = do
  InferEnv ident rec glob <- get
  let (newTyp, maxIdent) = shiftIds ident $ ntType typ
  put $ InferEnv (maxIdent + 1) rec glob
  return newTyp

getHighestId :: Type -> Int
getHighestId (AtomicType _) = 0
getHighestId (FunctionType t1 t2) = max (getHighestId t1) (getHighestId t2)
getHighestId (SumType t1 t2) = max (getHighestId t1) (getHighestId t2)
getHighestId (ProductType t1 t2) = max (getHighestId t1) (getHighestId t2)
getHighestId (ListType typ) = getHighestId typ
getHighestId (GenericType n) = n

shiftIds :: Int -> Type -> (Type, Int)
shiftIds ident (AtomicType a) = (AtomicType a, ident)
shiftIds ident (FunctionType t1 t2) =
  let (newT1, maxIdent1) = shiftIds ident t1
      (newT2, maxIdent2) = shiftIds ident t2
   in (FunctionType newT1 newT2, max maxIdent1 maxIdent2)
shiftIds ident (SumType t1 t2) =
  let (newT1, maxIdent1) = shiftIds ident t1
      (newT2, maxIdent2) = shiftIds ident t2
   in (SumType newT1 newT2, max maxIdent1 maxIdent2)
shiftIds ident (ProductType t1 t2) =
  let (newT1, maxIdent1) = shiftIds ident t1
      (newT2, maxIdent2) = shiftIds ident t2
   in (ProductType newT1 newT2, max maxIdent1 maxIdent2)
shiftIds ident (ListType typ) =
  let (typ', maxIdent) = shiftIds ident typ
   in (ListType typ', maxIdent)
shiftIds ident (GenericType gid) = (GenericType (gid + ident), gid + ident)

addNewSubstitution ::
  Int ->
  Type ->
  State ReconcileEnv (Either TypeErrorType ())
addNewSubstitution genericId typ = do
  ReconcileEnv env <- get
  let substituted = checkAndSubstitute env typ
  if checkSelfRefs genericId substituted
    then
      return
        $ Left
        $ TeSelfReferenceFound genericId
        $ show substituted
    else do
      let newEnv = substituteOldSubs genericId substituted env
      put $ ReconcileEnv ((genericId, substituted) : newEnv)
      return $ Right ()
 where
  -- 1. Check for and substitute references in the new substitution
  checkAndSubstitute :: [(Int, Type)] -> Type -> Type
  checkAndSubstitute subs (GenericType genericId') = case lookup genericId' subs of
    Nothing -> GenericType genericId'
    Just typ' -> typ'
  checkAndSubstitute _ (AtomicType atomicType) = AtomicType atomicType
  checkAndSubstitute subs (FunctionType paramType returnType) =
    FunctionType
      (checkAndSubstitute subs paramType)
      (checkAndSubstitute subs returnType)
  checkAndSubstitute subs (SumType type1 type2) =
    SumType
      (checkAndSubstitute subs type1)
      (checkAndSubstitute subs type2)
  checkAndSubstitute subs (ProductType type1 type2) =
    ProductType
      (checkAndSubstitute subs type1)
      (checkAndSubstitute subs type2)
  checkAndSubstitute subs (ListType typ') =
    ListType (checkAndSubstitute subs typ')
  -- 2. Check for self references in the new substitution (fail if found)
  checkSelfRefs :: Int -> Type -> Bool
  checkSelfRefs genericId' (GenericType genericId'') = genericId' == genericId''
  checkSelfRefs _ (AtomicType _) = False
  checkSelfRefs genericId' (FunctionType paramType returnType) =
    checkSelfRefs genericId' paramType || checkSelfRefs genericId' returnType
  checkSelfRefs genericId' (SumType type1 type2) =
    checkSelfRefs genericId' type1 || checkSelfRefs genericId' type2
  checkSelfRefs genericId' (ProductType type1 type2) =
    checkSelfRefs genericId' type1 || checkSelfRefs genericId' type2
  checkSelfRefs genericId' (ListType typ') = checkSelfRefs genericId' typ'
  -- 3. Check for and substitute references in the old substitutions
  substituteSimple :: Int -> Type -> Type -> Type
  substituteSimple genericId' typ' (GenericType genericId'') =
    if genericId' == genericId''
      then typ'
      else GenericType genericId''
  substituteSimple _ _ (AtomicType atomicType) = AtomicType atomicType
  substituteSimple genericId' typ' (FunctionType paramType returnType) =
    FunctionType
      (substituteSimple genericId' typ' paramType)
      (substituteSimple genericId' typ' returnType)
  substituteSimple genericId' typ' (SumType type1 type2) =
    SumType
      (substituteSimple genericId' typ' type1)
      (substituteSimple genericId' typ' type2)
  substituteSimple genericId' typ' (ProductType type1 type2) =
    ProductType
      (substituteSimple genericId' typ' type1)
      (substituteSimple genericId' typ' type2)
  substituteSimple genericId' typ' (ListType typ'') =
    ListType (substituteSimple genericId' typ' typ'')
  substituteOldSubs :: Int -> Type -> [(Int, Type)] -> [(Int, Type)]
  substituteOldSubs genericId' typ' =
    map (second (substituteSimple genericId' typ'))

addNewSubstitutionI :: Int -> Type -> State InferEnv (Either TypeErrorType ())
addNewSubstitutionI genericId typ = do
  InferEnv count renv glob <- get
  let (newREnv, result) = runState (addNewSubstitution genericId typ) renv
  put $ InferEnv count newREnv glob
  return result

updateWithSubstitutions :: Type -> State ReconcileEnv Type
updateWithSubstitutions typ = do
  ReconcileEnv env <- get
  return $ updateWithSubstitutions' env typ
 where
  updateWithSubstitutions' :: [(Int, Type)] -> Type -> Type
  updateWithSubstitutions' subs (GenericType genericId) = case lookup genericId subs of
    Nothing -> GenericType genericId
    Just typ' -> typ'
  updateWithSubstitutions' _ (AtomicType atomicType) = AtomicType atomicType
  updateWithSubstitutions' subs (FunctionType paramType returnType) =
    FunctionType
      (updateWithSubstitutions' subs paramType)
      (updateWithSubstitutions' subs returnType)
  updateWithSubstitutions' subs (SumType type1 type2) =
    SumType
      (updateWithSubstitutions' subs type1)
      (updateWithSubstitutions' subs type2)
  updateWithSubstitutions' subs (ProductType type1 type2) =
    ProductType
      (updateWithSubstitutions' subs type1)
      (updateWithSubstitutions' subs type2)
  updateWithSubstitutions' subs (ListType typ') =
    ListType (updateWithSubstitutions' subs typ')

updateWithSubstitutionsI :: Type -> State InferEnv Type
updateWithSubstitutionsI typ = do
  InferEnv count renv glob <- get
  let (newREnv, newTyp) = runState (updateWithSubstitutions typ) renv
  put $ InferEnv count newREnv glob
  return newTyp

reconcileTypesS ::
  Type ->
  Type ->
  State ReconcileEnv (Either TypeErrorType Type)
reconcileTypesS (GenericType genericId) (GenericType genericId')
  | genericId == genericId' = return $ Right (GenericType genericId)
reconcileTypesS (GenericType genericId) typ = do
  added <- addNewSubstitution genericId typ
  case added of
    Left e -> return $ Left e
    Right _ -> return $ Right typ
reconcileTypesS typ (GenericType genericId) = do
  added <- addNewSubstitution genericId typ
  case added of
    Left e -> return $ Left e
    Right _ -> return $ Right typ
reconcileTypesS (AtomicType atomicType) (AtomicType atomicType') =
  if atomicType == atomicType'
    then return $ Right (AtomicType atomicType)
    else
      return
        $ Left
        $ TeAtomicTypeMismatch (show atomicType) (show atomicType')
reconcileTypesS
  (FunctionType paramType returnType)
  (FunctionType paramType' returnType') = do
    reconciledParam <- reconcileTypesS paramType paramType'
    updatedReturn <- updateWithSubstitutions returnType
    updatedReturn' <- updateWithSubstitutions returnType'
    reconciledReturn <- reconcileTypesS updatedReturn updatedReturn'
    case (reconciledParam, reconciledReturn) of
      (Left e, _) -> return $ Left e
      (_, Left e) -> return $ Left e
      (Right reconciledParam', Right reconciledReturn') -> do
        updatedReconciledReturn <- updateWithSubstitutions reconciledReturn'
        return $ Right (FunctionType reconciledParam' updatedReconciledReturn)
reconcileTypesS (ProductType t1 t2) (ProductType t3 t4) = do
  firsts <- reconcileTypesS t1 t3
  ut2 <- updateWithSubstitutions t2
  ut4 <- updateWithSubstitutions t4
  seconds <- reconcileTypesS ut2 ut4
  case (firsts, seconds) of
    (Left e, _) -> return $ Left e
    (_, Left e) -> return $ Left e
    (Right firsts', Right seconds') -> do
      ufirsts <- updateWithSubstitutions firsts'
      return $ Right $ ProductType ufirsts seconds'
reconcileTypesS (SumType t1 t2) (SumType t3 t4) = do
  firsts <- reconcileTypesS t1 t3
  ut2 <- updateWithSubstitutions t2
  ut4 <- updateWithSubstitutions t4
  seconds <- reconcileTypesS ut2 ut4
  case (firsts, seconds) of
    (Left e, _) -> return $ Left e
    (_, Left e) -> return $ Left e
    (Right firsts', Right seconds') -> do
      ufirsts <- updateWithSubstitutions firsts'
      return $ Right $ SumType ufirsts seconds'
reconcileTypesS (ListType t1) (ListType t2) = do
  rec <- reconcileTypesS t1 t2
  case rec of
    Left e -> return $ Left e
    Right rec' -> do
      return $ Right $ ListType rec'
reconcileTypesS typ typ' =
  return
    $ Left
    $ TeTypeMismatch (show typ) (show typ')

reconcileTypesIS :: Type -> Type -> State InferEnv (Either TypeErrorType Type)
reconcileTypesIS t1 t2 = do
  InferEnv count renv glob <- get
  let (newREnv, reconciledM) = runState (reconcileTypesS t1 t2) renv
  case reconciledM of
    Left e -> return $ Left e
    Right reconciled -> do
      put $ InferEnv count newREnv glob
      return $ Right reconciled

checkTypeS ::
  Int -> Type -> Type -> State ReconcileEnv (Either TypeErrorType ())
checkTypeS _ (GenericType genericId) (GenericType genericId')
  | genericId == genericId' = return $ Right ()
checkTypeS hi typ (GenericType genericId)
  | genericId > hi =
      return
        $ Left
        $ TeCheckError (show (GenericType genericId))
        $ show typ
checkTypeS _ typ (GenericType genericId) = do
  addedE <- addNewSubstitution genericId typ
  case addedE of
    Left _ ->
      return
        $ Left
        $ TeCheckError (show (GenericType genericId))
        $ show typ
    Right _ -> return $ Right ()
checkTypeS _ (GenericType genericId) typ =
  return
    $ Left
    $ TeCheckError (show typ) (show $ GenericType genericId)
checkTypeS _ (AtomicType atomicType) (AtomicType atomicType') =
  if atomicType == atomicType'
    then return $ Right ()
    else
      return
        $ Left
        $ TeCheckError (show atomicType) (show atomicType')
checkTypeS
  hi
  (FunctionType paramType returnType)
  (FunctionType paramType' returnType') = do
    checkedParam <- checkTypeS hi paramType paramType'
    updatedReturn <- updateWithSubstitutions returnType
    updatedReturn' <- updateWithSubstitutions returnType'
    checkedReturn <- checkTypeS hi updatedReturn updatedReturn'
    case (checkedParam, checkedReturn) of
      (Left e, _) -> return $ Left e
      (_, Left e) -> return $ Left e
      (Right _, Right _) -> return $ Right ()
checkTypeS hi (ProductType t1 t2) (ProductType t3 t4) = do
  firsts <- checkTypeS hi t1 t3
  t2' <- updateWithSubstitutions t2
  t4' <- updateWithSubstitutions t4
  seconds <- checkTypeS hi t2' t4'
  case (firsts, seconds) of
    (Left e, _) -> return $ Left e
    (_, Left e) -> return $ Left e
    (Right _, Right _) -> return $ Right ()
checkTypeS hi (SumType t1 t2) (SumType t3 t4) = do
  firsts <- checkTypeS hi t1 t3
  t2' <- updateWithSubstitutions t2
  t4' <- updateWithSubstitutions t4
  seconds <- checkTypeS hi t2' t4'
  case (firsts, seconds) of
    (Left e, _) -> return $ Left e
    (_, Left e) -> return $ Left e
    (Right _, Right _) -> return $ Right ()
checkTypeS hi (ListType t1) (ListType t2) = checkTypeS hi t1 t2
checkTypeS _ typ typ' =
  return
    $ Left
    $ TeCheckError (show typ) (show typ')

checkType :: MutExcTy2 -> Either TypeErrorType ()
checkType excTys =
  execState
    (checkTypeS highestIdT2 st1 (met2Snd excTys))
    (ReconcileEnv [])
 where
  (st1, _) = shiftIds highestIdT2 (ntType $ met2Fst excTys)
  highestIdT2 = getHighestId (met2Snd excTys)

-- Also returns the last element of the list
createGenericList :: Int -> State InferEnv ([Type], Type)
createGenericList 0 = error "createGenericList called with 0"
createGenericList n = do
  start <- replicateM (n - 1) (GenericType <$> getNewId)
  lastId <- getNewId
  return (start ++ [GenericType lastId], GenericType lastId)

getNewId :: State InferEnv Int
getNewId = do
  InferEnv ident rec glob <- get
  put $ InferEnv (ident + 1) rec glob
  return ident

getTypeOfGlobal :: L.VarIdent -> State InferEnv Type
getTypeOfGlobal gname = do
  InferEnv _ rec glob <- get
  case lookup gname glob of
    Nothing -> do
      newId <- getNewId
      (InferEnv ident _ _) <- get
      put $ InferEnv ident rec $ (gname, newId) : glob
      return $ GenericType newId
    Just recId -> do
      let ReconcileEnv recList = rec
      case lookup recId recList of
        Nothing -> return $ GenericType recId
        Just typ -> return typ
