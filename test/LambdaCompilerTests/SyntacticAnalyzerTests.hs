module LambdaCompilerTests.SyntacticAnalyzerTests (spec) where

import qualified Lexer.Internal as L
import SyntacticAnalyzer.Internal
import Test.Hspec

spec :: Spec
spec = do
  describe "type parser" $ do
    it "can parse simple types" $ do
      L.execParser type_ "a -> b"
        `shouldBe` Right (FunctionType (TypeId (L.TypIdent "a")) (TypeId (L.TypIdent "b")))
      L.execParser type_ "a * b"
        `shouldBe` Right (ProductType (TypeId (L.TypIdent "a")) (TypeId (L.TypIdent "b")))
      L.execParser type_ "a + b"
        `shouldBe` Right (SumType (TypeId (L.TypIdent "a")) (TypeId (L.TypIdent "b")))
      L.execParser type_ "a * b * c"
        `shouldBe` Right
          ( ProductType
              (TypeId (L.TypIdent "a"))
              (ProductType (TypeId (L.TypIdent "b")) (TypeId (L.TypIdent "c")))
          )
  describe "sum type" $ do
    it "has higher precedence than function types" $ do
      L.execParser type_ "a + b -> c"
        `shouldBe` Right
          ( FunctionType
              (SumType (TypeId (L.TypIdent "a")) (TypeId (L.TypIdent "b")))
              (TypeId (L.TypIdent "c"))
          )
  describe "product type" $ do
    it "has higher precedence than function types" $ do
      L.execParser type_ "a * b -> c"
        `shouldBe` Right
          ( FunctionType
              (ProductType (TypeId (L.TypIdent "a")) (TypeId (L.TypIdent "b")))
              (TypeId (L.TypIdent "c"))
          )
    it "has higher precedence than sum types" $ do
      L.execParser type_ "a * b + c"
        `shouldBe` Right
          ( SumType
              (ProductType (TypeId (L.TypIdent "a")) (TypeId (L.TypIdent "b")))
              (TypeId (L.TypIdent "c"))
          )
      
