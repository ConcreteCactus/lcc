module LambdaCompilerTests.SemanticAnalyzer.TypeTest (spec) where

import SemanticAnalyzer.Type.Internal
import Test.Hspec

spec :: Spec
spec = do
  describe "mkNormType" $ do
    it "doesn't touch already good numberings" $ do
      mkNormType (GenericType 1) `shouldBe` NormType (GenericType 1) 1
      mkNormType (FunctionType (GenericType 1) (GenericType 2)) `shouldBe` NormType (FunctionType (GenericType 1) (GenericType 2)) 2
