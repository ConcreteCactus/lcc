module LambdaCompilerTests.SemanticAnalyzer.TypeTest (spec) where

import SemanticAnalyzer.Type.Internal
import Test.Hspec
import qualified SyntacticAnalyzer as Y

spec :: Spec
spec = do
  describe "mkNormType" $ do
    it "doesn't touch already good numberings" $ do
      mkNormType (GenericType 1) `shouldBe` NormType (GenericType 1) 1
      mkNormType (FunctionType (GenericType 1) (GenericType 2))
        `shouldBe` NormType (FunctionType (GenericType 1) (GenericType 2)) 2
  describe "checkType" $ do
    it "can handle simple types" $ do
      isRight (checkType (mkMutExcTy2 (mkn $ gt 1) (mkn $ GenericType 1)))
        `shouldBe` True
      isRight (checkType (mkMutExcTy2 (mkn at) (mkn at)))
        `shouldBe` True
      isRight (checkType (mkMutExcTy2 (mkn $ gt 1) (mkn at)))
        `shouldBe` False
      isRight (checkType (mkMutExcTy2 (mkn at) (mkn $ gt 1)))
        `shouldBe` True
    it "rejects ambiguity" $ do
      isRight
        ( checkType
            ( mkMutExcTy2
                (mkn $ ft at (gt 1))
                (mkn $ ft (gt 1) (gt 1))
            )
        )
        `shouldBe` False
    it "can handle function types" $ do
      isRight (checkType (mkMutExcTy2 
        (mkn $ ft (gt 1) (ft (gt 1) (gt 1)))
        (mkn $ ft (gt 1) (ft (gt 2) (gt 1)))))
        `shouldBe` True

mkn :: Type -> NormType
mkn = mkNormType

gt :: Int -> Type
gt = GenericType

at :: Type
at = AtomicType Y.AI32

ft :: Type -> Type -> Type
ft = FunctionType

isRight :: Either a b -> Bool
isRight (Right _) = True
isRight _ = False
