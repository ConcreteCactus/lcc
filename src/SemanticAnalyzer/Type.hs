{-# LANGUAGE TupleSections #-}

module SemanticAnalyzer.Type
  ( Type (..),
    NormType,
    AtomicType (..),
    InferEnv (..),
    ReconcileEnv (..),
    ntType,
    ntMaxId,
    convertType,
    shiftNewIds,
    mkMutExcTy2,
    met2Fst,
    met2Snd,
    addNewSubstitutionI,
    updateWithSubstitutionsI,
    mkNormType,
    reconcileTypesS,
    reconcileTypesIS,
    checkType,
    createGenericList,
    getNewId,
    getTypeInferredToRefSI,
    getAllTypesInferredToRefsSI,
    semanticAnalyzerSemTypeTests,
  )
where

import Control.Monad
import Data.Bifunctor
import Errors
import qualified Lexer as L
import qualified SyntacticAnalyzer as Y
import Util

data AtomicType = AInt deriving (Show, Eq)

data Type
  = AtomicType AtomicType
  | GenericType Int
  | FunctionType Type Type
  deriving (Eq)

data NormType = NormType
  { ntType :: Type,
    ntMaxId :: Int
  }
  deriving (Eq)

instance Show Type where
  show (AtomicType a) = show a
  show (GenericType n) = "T" ++ show n
  show (FunctionType t1@(FunctionType _ _) t2) = "(" ++ show t1 ++ ") -> " ++ show t2
  show (FunctionType t1 t2) = show t1 ++ " -> " ++ show t2

instance Show NormType where
  show nt = show $ ntType nt

-- Mutually exclusive types (they don't share generic ids)
data MutExcTy2 = MutExcTy2
  { met2Fst :: NormType,
    met2Snd :: Type
  }

data InferEnv = InferEnv Int ReconcileEnv [(L.Ident, Int)]

-- Laws
--   1. forall (a, _) in RE : forall (_, b) in RE : forall genericId in b : a != genericId
--   2. forall (a, b), (a', b') in RE : b != b' => a != a'
newtype ReconcileEnv = ReconcileEnv [(Int, Type)]

data ConvertEnv a = ConvertEnv
  { ceNextId :: Int,
    ceDict :: [(a, Int)]
  }

convertType :: Y.Type -> NormType
convertType synType = NormType typ (nextId - 1)
  where
    (ConvertEnv nextId _, typ) = runState (convertTypeS synType) (ConvertEnv 1 [])

convertTypeS :: Y.Type -> State (ConvertEnv L.Ident) Type
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

mkNormType :: Type -> NormType
mkNormType typ = NormType typ' (maxId - 1)
  where
    (ConvertEnv maxId _, typ') = runState (mkNormTypeS typ) (ConvertEnv 1 [])

mkNormTypeS :: Type -> State (ConvertEnv Int) Type
mkNormTypeS (GenericType ind) = do
  env <- get
  case lookup ind (ceDict env) of
    Nothing -> do
      put $
        ConvertEnv
          { ceNextId = ceNextId env + 1,
            ceDict = (ind, ceNextId env) : ceDict env
          }
      return $ GenericType (ceNextId env + 1)
    Just ind' -> return $ GenericType ind'
mkNormTypeS (AtomicType atomicType) = return $ AtomicType atomicType
mkNormTypeS (FunctionType paramType returnType) = do
  normalizedParam <- mkNormTypeS paramType
  normalizedReturn <- mkNormTypeS returnType
  return $ FunctionType normalizedParam normalizedReturn

mkMutExcTy2 :: NormType -> NormType -> MutExcTy2
mkMutExcTy2 original shiftCandidate =
  MutExcTy2 original $
    fst $
      shiftIds (ntMaxId original + 1) $
        ntType shiftCandidate

shiftNewIds :: Type -> State InferEnv Type
shiftNewIds typ = do
  InferEnv ident rec refs <- get
  let (newTyp, maxIdent) = shiftIds ident typ
  put $ InferEnv (maxIdent + 1) rec refs
  return newTyp

getHighestId :: Type -> Int
getHighestId (AtomicType _) = 0
getHighestId (FunctionType t1 t2) = max (getHighestId t1) (getHighestId t2)
getHighestId (GenericType n) = n

shiftIds :: Int -> Type -> (Type, Int)
shiftIds ident (AtomicType a) = (AtomicType a, ident)
shiftIds ident (FunctionType t1 t2) =
  let (newT1, maxIdent1) = shiftIds ident t1
      (newT2, maxIdent2) = shiftIds ident t2
   in (FunctionType newT1 newT2, max maxIdent1 maxIdent2)
shiftIds ident (GenericType gid) = (GenericType (gid + ident), gid + ident)

addNewSubstitution :: Int -> Type -> State ReconcileEnv (Either STypeError ())
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
    checkAndSubstitute :: [(Int, Type)] -> Type -> Type
    checkAndSubstitute subs (GenericType genericId') = case lookup genericId' subs of
      Nothing -> GenericType genericId'
      Just typ' -> typ'
    checkAndSubstitute _ (AtomicType atomicType) = AtomicType atomicType
    checkAndSubstitute subs (FunctionType paramType returnType) =
      FunctionType (checkAndSubstitute subs paramType) (checkAndSubstitute subs returnType)
    -- 2. Check for self references in the new substitution (fail if found)
    checkSelfRefs :: Int -> Type -> Bool
    checkSelfRefs genericId' (GenericType genericId'') = genericId' == genericId''
    checkSelfRefs _ (AtomicType _) = False
    checkSelfRefs genericId' (FunctionType paramType returnType) =
      checkSelfRefs genericId' paramType || checkSelfRefs genericId' returnType
    -- 3. Check for and substitute references in the old substitutions
    substituteSimple :: Int -> Type -> Type -> Type
    substituteSimple genericId' typ' (GenericType genericId'') =
      if genericId' == genericId''
        then typ'
        else GenericType genericId''
    substituteSimple _ _ (AtomicType atomicType) = AtomicType atomicType
    substituteSimple genericId' typ' (FunctionType paramType returnType) =
      FunctionType (substituteSimple genericId' typ' paramType) (substituteSimple genericId' typ' returnType)
    substituteOldSubs :: Int -> Type -> [(Int, Type)] -> [(Int, Type)]
    substituteOldSubs genericId' typ' =
      map (second (substituteSimple genericId' typ'))

addNewSubstitutionI :: Int -> Type -> State InferEnv (Either STypeError ())
addNewSubstitutionI genericId typ = do
  InferEnv count renv refs <- get
  let (newREnv, result) = runState (addNewSubstitution genericId typ) renv
  put $ InferEnv count newREnv refs
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
      FunctionType (updateWithSubstitutions' subs paramType) (updateWithSubstitutions' subs returnType)

updateWithSubstitutionsI :: Type -> State InferEnv Type
updateWithSubstitutionsI typ = do
  InferEnv count renv refs <- get
  let (newREnv, newTyp) = runState (updateWithSubstitutions typ) renv
  put $ InferEnv count newREnv refs
  return newTyp

reconcileTypesS :: Type -> Type -> State ReconcileEnv (Either STypeError Type)
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
    else return $ Left $ STAtomicTypeMismatch (show atomicType) (show atomicType')
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
      (Right reconciledParam', Right reconciledReturn') ->
        return $ Right (FunctionType reconciledParam' reconciledReturn')
reconcileTypesS typ typ' = return $ Left $ STTypeMismatch (show typ) (show typ')

checkTypeS :: Int -> Type -> Type -> State ReconcileEnv (Either STypeError ())
checkTypeS _ (GenericType genericId) (GenericType genericId')
  | genericId == genericId' = return $ Right ()
checkTypeS hi typ (GenericType genericId)
  | genericId > hi = return $ Left $ STTypeMismatch (show (GenericType genericId)) $ show typ
checkTypeS _ typ (GenericType genericId) = do
  added <- addNewSubstitution genericId typ
  case added of
    Left _ -> return $ Left $ STTypeMismatch (show (GenericType genericId)) $ show typ
    Right _ -> return $ Right ()
checkTypeS _ (GenericType genericId) typ =
  return $
    Left $
      STTypeMismatch (show typ) (show $ GenericType genericId)
checkTypeS _ (AtomicType atomicType) (AtomicType atomicType') =
  if atomicType == atomicType'
    then return $ Right ()
    else return $ Left $ STAtomicTypeMismatch (show atomicType) (show atomicType')
checkTypeS hi (FunctionType paramType returnType) (FunctionType paramType' returnType') = do
  checkedParam <- checkTypeS hi paramType paramType'
  updatedReturn' <- updateWithSubstitutions returnType'
  checkedReturn <- checkTypeS hi returnType updatedReturn'
  case (checkedParam, checkedReturn) of
    (Left e, _) -> return $ Left e
    (_, Left e) -> return $ Left e
    (Right _, Right _) -> return $ Right ()
checkTypeS _ typ typ' = return $ Left $ STTypeMismatch (show typ) (show typ')

reconcileTypesIS :: Type -> Type -> State InferEnv (Either STypeError Type)
reconcileTypesIS t1 t2 = do
  InferEnv count renv refs <- get
  let (newREnv, reconciledM) = runState (reconcileTypesS t1 t2) renv
  case reconciledM of
    Left e -> return $ Left e
    Right reconciled -> do
      put $ InferEnv count newREnv refs
      return $ Right reconciled

checkType :: MutExcTy2 -> Either STypeError ()
checkType excTys = execState (checkTypeS highestIdT2 st1 (met2Snd excTys)) (ReconcileEnv [])
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
  InferEnv ident rec refs <- get
  put $ InferEnv (ident + 1) rec refs
  return ident

getOrCreateIdOfRefSI :: L.Ident -> State InferEnv Int
getOrCreateIdOfRefSI refName = do
  InferEnv ident rec refs <- get
  case lookup refName refs of
    Nothing -> do
      refId <- getNewId
      put $ InferEnv ident rec ((refName, refId) : refs)
      return refId
    Just refId -> return refId

getTypeInferredToRefSI :: L.Ident -> State InferEnv Type
getTypeInferredToRefSI refName = do
  refId <- getOrCreateIdOfRefSI refName
  updateWithSubstitutionsI (GenericType refId)

getAllTypesInferredToRefsSI :: State InferEnv [(L.Ident, Type)]
getAllTypesInferredToRefsSI = do
  InferEnv _ _ refs <- get
  mapM (\(refName, _) -> (refName,) <$> getTypeInferredToRefSI refName) refs

-- Unit tests

semanticAnalyzerSemTypeTests :: [Bool]
semanticAnalyzerSemTypeTests =
  [ checkType (MutExcTy2 (na AInt) (a AInt)) == Right (),
    checkType (MutExcTy2 (na AInt) (t 1)) == Right (),
    checkType (MutExcTy2 (mn $ t 1) (a AInt)) == Left (STTypeMismatch "AInt" "T1"),
    checkType (MutExcTy2 (mn $ t 1 --> t 2) (t 1 --> t 2)) == Right (),
    checkType (MutExcTy2 (mn $ t 1 --> t 2) (t 1 --> t 3)) == Right (),
    checkType (MutExcTy2 (mn $ t 1 --> t 2) (t 1 --> t 1)) == Left (STTypeMismatch "T2" "T3"),
    checkType (MutExcTy2 (mn $ t 1 --> a AInt) (t 1 --> t 1)) == Left (STTypeMismatch "T2" "AInt"),
    checkType (MutExcTy2 (mn $ t 1 --> t 2) (t 3 --> t 2)) == Right ()
  ]
  where
    (-->) = FunctionType
    t = GenericType
    a = AtomicType
    na at = NormType (AtomicType at) 0
    mn = mkNormType