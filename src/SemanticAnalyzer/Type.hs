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
    getTypeOfGlobal,
    -- semanticAnalyzerSemTypeTests,
  )
where

import SemanticAnalyzer.Type.Internal
