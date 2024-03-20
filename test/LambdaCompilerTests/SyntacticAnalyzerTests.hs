module LambdaCompilerTests.SyntacticAnalyzerTests (spec) where

import AtomicType
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
  describe "expression parser" $ do
    it "can parse a simple lambda" $ do
      L.execParser expression "\\x.x"
        `shouldBe` Right
          (Lambda (L.VarIdent "x") (Id (L.VarIdent "x")))
    it "can parse a simple application" $ do
      L.execParser expression "(\\x.x) y"
        `shouldBe` Right
          ( Application
              (Lambda (L.VarIdent "x") $ Id (L.VarIdent "x"))
              (Id $ L.VarIdent "y")
          )
    it "can parse a simple ifThenElse" $
      do
        L.execParser expression "if 0bool then x else y"
        `shouldBe` Right
          ( IfThenElse
              (Lit $ L.Literal "0" ABool)
              (Id $ L.VarIdent "x")
              (Id $ L.VarIdent "y")
          )
    it "can parse expression with good whitespace" $
      do
        L.execParser expression "if 0bool\n then x else y"
        `shouldBe` Right
          ( IfThenElse
              (Lit $ L.Literal "0" ABool)
              (Id $ L.VarIdent "x")
              (Id $ L.VarIdent "y")
          )
    it "can't parse expression with bad whitespace" $
      do
        isRight (L.execParser expression "if 0bool\nthen x else y")
        `shouldBe` False
    it "lambdas are right associative" $ do
      L.execParser expression "\\x.\\x.\\x.x"
        `shouldBe` Right
          ( Lambda (L.VarIdent "x") $
              Lambda (L.VarIdent "x") $
                Lambda (L.VarIdent "x") $
                  Id (L.VarIdent "x")
          )

isRight :: Either a b -> Bool
isRight (Left _) = False
isRight (Right _) = True
