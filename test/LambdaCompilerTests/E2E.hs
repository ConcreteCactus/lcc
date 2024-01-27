{-# LANGUAGE FlexibleInstances #-}

module LambdaCompilerTests.E2E (spec) where

import Test.Hspec
import Test.QuickCheck
import Util

spec :: Spec
spec = return ()

newtype ProgramGenEnv = ProgramGenEnv {pgeCtr :: Int}
type PGen a = State ProgramGenEnv (Gen a)

incEnvCtr :: State ProgramGenEnv Int
incEnvCtr = do
  env <- get
  let ctr = pgeCtr env
  put env{pgeCtr = ctr + 1}
  return ctr

genLcIdent :: PGen String
genLcIdent = do
    ctr <- incEnvCtr
    let identName = do
          headChar <- choose ('a', 'z')
          tailCharCount <- choose (0, 20)
          tailChars <-
            vectorOf
              tailCharCount
              (oneof [choose ('a', 'z'), choose ('A', 'Z')])
          return $ headChar : tailChars
    return $ (++show ctr) <$> identName

