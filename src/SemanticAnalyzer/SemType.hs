{-# LANGUAGE TupleSections #-}

module SemanticAnalyzer.SemType
  ( SemType (..),
    AtomicType (..),
    InferEnv (..),
    ReconcileEnv (..),
    createSemanticType,
    shiftNewIds,
    shiftAwayFrom,
    addNewSubstitutionI,
    updateWithSubstitutionsI,
    normalizeGenericIndices,
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
import SyntacticAnalyzer
import Util

data AtomicType = AInt deriving (Show, Eq)

data SemType
  = SAtomicType AtomicType
  | SGenericType Int
  | SFunctionType SemType SemType
  deriving (Eq)

instance Show SemType where
  show (SAtomicType a) = show a
  show (SGenericType n) = "T" ++ show n
  show (SFunctionType t1@(SFunctionType _ _) t2) = "(" ++ show t1 ++ ") -> " ++ show t2
  show (SFunctionType t1 t2) = show t1 ++ " -> " ++ show t2

data InferEnv = InferEnv Int ReconcileEnv [(String, Int)]

-- Laws
--   1. forall (a, _) in RE : forall (_, b) in RE : forall genericId in b : a != genericId
--   2. forall (a, b), (a', b') in RE : b != b' => a != a'
newtype ReconcileEnv = ReconcileEnv [(Int, SemType)]

data SemanticTypeEnv = SemanticTypeEnv Int [(String, Int)]

createSemanticType :: SynTypeExpression -> SemType
createSemanticType synType = execState (createSemanticTypeS synType) (SemanticTypeEnv 1 [])

createSemanticTypeS :: SynTypeExpression -> State SemanticTypeEnv SemType
createSemanticTypeS (TypeId id') = do
  SemanticTypeEnv ident decls <- get
  case lookup id' decls of
    Just ident' -> return $ SGenericType ident'
    Nothing -> do
      put $ SemanticTypeEnv (ident + 1) ((id', ident) : decls)
      return $ SGenericType ident
createSemanticTypeS (FunctionType t1 t2) = do
  t1' <- createSemanticTypeS t1
  t2' <- createSemanticTypeS t2
  return $ SFunctionType t1' t2'

shiftAwayFrom :: SemType -> SemType -> SemType
shiftAwayFrom original shiftCandidate =
  fst $ shiftIds (getHighestId original + 1) shiftCandidate

shiftNewIds :: SemType -> State InferEnv SemType
shiftNewIds typ = do
  InferEnv ident rec refs <- get
  let (newTyp, maxIdent) = shiftIds ident typ
  put $ InferEnv (maxIdent + 1) rec refs
  return newTyp

getHighestId :: SemType -> Int
getHighestId (SAtomicType _) = 0
getHighestId (SFunctionType t1 t2) = max (getHighestId t1) (getHighestId t2)
getHighestId (SGenericType n) = n

shiftIds :: Int -> SemType -> (SemType, Int)
shiftIds ident (SAtomicType a) = (SAtomicType a, ident)
shiftIds ident (SFunctionType t1 t2) =
  let (newT1, maxIdent1) = shiftIds ident t1
      (newT2, maxIdent2) = shiftIds ident t2
   in (SFunctionType newT1 newT2, max maxIdent1 maxIdent2)
shiftIds ident (SGenericType gid) = (SGenericType (gid + ident), gid + ident)

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
    substituteSimple :: Int -> SemType -> SemType -> SemType
    substituteSimple genericId' typ' (SGenericType genericId'') =
      if genericId' == genericId''
        then typ'
        else SGenericType genericId''
    substituteSimple _ _ (SAtomicType atomicType) = SAtomicType atomicType
    substituteSimple genericId' typ' (SFunctionType paramType returnType) =
      SFunctionType (substituteSimple genericId' typ' paramType) (substituteSimple genericId' typ' returnType)
    substituteOldSubs :: Int -> SemType -> [(Int, SemType)] -> [(Int, SemType)]
    substituteOldSubs genericId' typ' =
      map (second (substituteSimple genericId' typ'))

addNewSubstitutionI :: Int -> SemType -> State InferEnv (Either STypeError ())
addNewSubstitutionI genericId typ = do
  InferEnv count renv refs <- get
  let (newREnv, result) = runState (addNewSubstitution genericId typ) renv
  put $ InferEnv count newREnv refs
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
  InferEnv count renv refs <- get
  let (newREnv, newTyp) = runState (updateWithSubstitutions typ) renv
  put $ InferEnv count newREnv refs
  return newTyp

reconcileTypesS :: SemType -> SemType -> State ReconcileEnv (Either STypeError SemType)
reconcileTypesS (SGenericType genericId) (SGenericType genericId')
  | genericId == genericId' = return $ Right (SGenericType genericId)
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

checkTypeS :: Int -> SemType -> SemType -> State ReconcileEnv (Either STypeError ())
checkTypeS _ (SGenericType genericId) (SGenericType genericId')
  | genericId == genericId' = return $ Right ()
checkTypeS hi typ (SGenericType genericId)
  | genericId > hi = return $ Left $ STTypeMismatch (show (SGenericType genericId)) $ show typ
checkTypeS _ typ (SGenericType genericId) = do
  added <- addNewSubstitution genericId typ
  case added of
    Left _ -> return $ Left $ STTypeMismatch (show (SGenericType genericId)) $ show typ
    Right _ -> return $ Right ()
checkTypeS _ (SGenericType genericId) typ =
  return $
    Left $
      STTypeMismatch (show typ) (show $ SGenericType genericId)
checkTypeS _ (SAtomicType atomicType) (SAtomicType atomicType') =
  if atomicType == atomicType'
    then return $ Right ()
    else return $ Left $ STAtomicTypeMismatch (show atomicType) (show atomicType')
checkTypeS hi (SFunctionType paramType returnType) (SFunctionType paramType' returnType') = do
  checkedParam <- checkTypeS hi paramType paramType'
  updatedReturn' <- updateWithSubstitutions returnType'
  checkedReturn <- checkTypeS hi returnType updatedReturn'
  case (checkedParam, checkedReturn) of
    (Left e, _) -> return $ Left e
    (_, Left e) -> return $ Left e
    (Right _, Right _) -> return $ Right ()
checkTypeS _ typ typ' = return $ Left $ STTypeMismatch (show typ) (show typ')

reconcileTypesIS :: SemType -> SemType -> State InferEnv (Either STypeError SemType)
reconcileTypesIS t1 t2 = do
  InferEnv count renv refs <- get
  let (newREnv, reconciledM) = runState (reconcileTypesS t1 t2) renv
  case reconciledM of
    Left e -> return $ Left e
    Right reconciled -> do
      put $ InferEnv count newREnv refs
      return $ Right reconciled

checkType :: SemType -> SemType -> Either STypeError ()
checkType t1 t2 = execState (checkTypeS highestIdT2 st1 t2) (ReconcileEnv [])
  where
    (st1, _) = shiftIds highestIdT2 t1
    highestIdT2 = getHighestId t2

-- Also returns the last element of the list
createGenericList :: Int -> State InferEnv ([SemType], SemType)
createGenericList 0 = error "createGenericList called with 0"
createGenericList n = do
  start <- replicateM (n - 1) (SGenericType <$> getNewId)
  lastId <- getNewId
  return (start ++ [SGenericType lastId], SGenericType lastId)

getNewId :: State InferEnv Int
getNewId = do
  InferEnv ident rec refs <- get
  put $ InferEnv (ident + 1) rec refs
  return ident

getOrCreateIdOfRefSI :: String -> State InferEnv Int
getOrCreateIdOfRefSI refName = do
  InferEnv ident rec refs <- get
  case lookup refName refs of
    Nothing -> do
      refId <- getNewId
      put $ InferEnv ident rec ((refName, refId) : refs)
      return refId
    Just refId -> return refId

getTypeInferredToRefSI :: String -> State InferEnv SemType
getTypeInferredToRefSI refName = do
  refId <- getOrCreateIdOfRefSI refName
  updateWithSubstitutionsI (SGenericType refId)

getAllTypesInferredToRefsSI :: State InferEnv [(String, SemType)]
getAllTypesInferredToRefsSI = do
  InferEnv _ _ refs <- get
  mapM (\(refName, _) -> (refName,) <$> getTypeInferredToRefSI refName) refs

normalizeGenericIndices :: SemType -> SemType
normalizeGenericIndices typ = execState (normalizeGenericIndicesS typ) []

normalizeGenericIndicesS :: SemType -> State [(Int, Int)] SemType
normalizeGenericIndicesS (SGenericType ind) = do
  dict <- get
  case lookup ind dict of
    Nothing -> do
      put $ (ind, length dict + 1) : dict
      return $ SGenericType (length dict + 1)
    Just ind' -> return $ SGenericType ind'
normalizeGenericIndicesS (SAtomicType atomicType) = return $ SAtomicType atomicType
normalizeGenericIndicesS (SFunctionType paramType returnType) = do
  normalizedParam <- normalizeGenericIndicesS paramType
  normalizedReturn <- normalizeGenericIndicesS returnType
  return $ SFunctionType normalizedParam normalizedReturn

-- Unit tests

semanticAnalyzerSemTypeTests :: [Bool]
semanticAnalyzerSemTypeTests =
  [ checkType (a AInt) (a AInt) == Right (),
    checkType (a AInt) (t 1) == Right (),
    checkType (t 1) (a AInt) == Left (STTypeMismatch "AInt" "T1"),
    checkType (t 1 --> t 2) (t 1 --> t 2) == Right (),
    checkType (t 1 --> t 2) (t 1 --> t 3) == Right (),
    checkType (t 1 --> t 2) (t 1 --> t 1) == Left (STTypeMismatch "T2" "T3"),
    checkType (t 1 --> a AInt) (t 1 --> t 1) == Left (STTypeMismatch "T2" "AInt"),
    checkType (t 1 --> t 2) (t 3 --> t 2) == Right ()
  ]
  where
    (-->) = SFunctionType
    t = SGenericType
    a = SAtomicType
