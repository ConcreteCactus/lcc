module SemanticAnalyzer.SemType
  ( SemType (..),
    AtomicType (..),
    InferEnv (..),
    ReconcileEnv (..),
    createSemanticType,
    shiftNewIds,
    addNewSubstitutionI,
    updateWithSubstitutionsI,
    normalizeGenericIndices,
    reconcileTypesIS,
    createGenericList,
    getNewId,
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

data InferEnv = InferEnv Int ReconcileEnv

-- Laws
--   1. forall (a, _) in RE : forall (_, b) in RE : forall genericId in b : a != genericId
--   2. forall (a, b), (a', b') in RE : b != b' => a != a'
newtype ReconcileEnv = ReconcileEnv [(Int, SemType)]

createSemanticType :: SynTypeExpression -> Either CompilerError SemType
createSemanticType (TypeId "int") = Right $ SAtomicType AInt
createSemanticType (FunctionType t1 t2) =
  SFunctionType
    <$> createSemanticType t1
    <*> createSemanticType t2
createSemanticType _ = Left CompilerError

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

reconcileTypesIS :: SemType -> SemType -> State InferEnv (Either STypeError SemType)
reconcileTypesIS t1 t2 = do
  InferEnv count renv <- get
  let (newREnv, reconciledM) = runState (reconcileTypesS t1 t2) renv
  case reconciledM of
    Left e -> return $ Left e
    Right reconciled -> do
      put $ InferEnv count newREnv
      return $ Right reconciled

-- Also returns the last element of the list
createGenericList :: Int -> State InferEnv ([SemType], SemType)
createGenericList 0 = error "createGenericList called with 0"
createGenericList n = do
  start <- replicateM (n - 1) (SGenericType <$> getNewId)
  lastId <- getNewId
  return (start ++ [SGenericType lastId], SGenericType lastId)

getNewId :: State InferEnv Int
getNewId = do
  InferEnv ident rec <- get
  put $ InferEnv (ident + 1) rec
  return ident

normalizeGenericIndices :: SemType -> SemType
normalizeGenericIndices typ = execState (normalizeGenericIndicesS typ) []

normalizeGenericIndicesS :: SemType -> State [(Int, Int)] SemType
normalizeGenericIndicesS (SGenericType ind) = do
  dict <- get
  case lookup ind dict of
    Nothing -> do
      put $ (ind, length dict) : dict
      return $ SGenericType (length dict)
    Just ind' -> return $ SGenericType ind'
normalizeGenericIndicesS (SAtomicType atomicType) = return $ SAtomicType atomicType
normalizeGenericIndicesS (SFunctionType paramType returnType) = do
  normalizedParam <- normalizeGenericIndicesS paramType
  normalizedReturn <- normalizeGenericIndicesS returnType
  return $ SFunctionType normalizedParam normalizedReturn
