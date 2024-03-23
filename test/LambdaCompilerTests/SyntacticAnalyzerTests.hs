module LambdaCompilerTests.SyntacticAnalyzerTests (spec) where

import AtomicType
import qualified Lexer.Internal as L
import SyntacticAnalyzer.Internal
import Test.Hspec

spec :: Spec
spec = do
    describe "type parser" $ do
        it "simple function types can be parsed" $ do
            L.execParser type_ "a -> b"
                `shouldBe` Right (FunctionType (TypeId (L.TypIdent "a")) (TypeId (L.TypIdent "b")))
        it "simple product types can be parsed" $ do
            L.execParser type_ "a * b"
                `shouldBe` Right (ProductType (TypeId (L.TypIdent "a")) (TypeId (L.TypIdent "b")))
        it "simple sum types can be parsed" $ do
            L.execParser type_ "a + b"
                `shouldBe` Right (SumType (TypeId (L.TypIdent "a")) (TypeId (L.TypIdent "b")))
        it
            "multiple chained product types can be parsed, and they are right-associative"
            $ do
                L.execParser type_ "a * b * c"
                    `shouldBe` Right
                        ( ProductType
                            (TypeId (L.TypIdent "a"))
                            (ProductType (TypeId (L.TypIdent "b")) (TypeId (L.TypIdent "c")))
                        )
    describe "sum types" $ do
        it "have higher precedence than function types" $ do
            L.execParser type_ "a + b -> c"
                `shouldBe` Right
                    ( FunctionType
                        (SumType (TypeId (L.TypIdent "a")) (TypeId (L.TypIdent "b")))
                        (TypeId (L.TypIdent "c"))
                    )
    describe "product types" $ do
        it "have higher precedence than function types" $ do
            L.execParser type_ "a * b -> c"
                `shouldBe` Right
                    ( FunctionType
                        (ProductType (TypeId (L.TypIdent "a")) (TypeId (L.TypIdent "b")))
                        (TypeId (L.TypIdent "c"))
                    )
        it "have higher precedence than sum types" $ do
            L.execParser type_ "a * b + c"
                `shouldBe` Right
                    ( SumType
                        (ProductType (TypeId (L.TypIdent "a")) (TypeId (L.TypIdent "b")))
                        (TypeId (L.TypIdent "c"))
                    )
    describe "expression parser" $ do
        it "simple lambda expressions can be parsed" $ do
            L.execParser expression "\\x.x"
                `shouldBe` Right
                    (Lambda (L.VarIdent "x") (Id (L.VarIdent "x")))
        it "simple applications can be parsed" $ do
            L.execParser expression "(\\x.x) y"
                `shouldBe` Right
                    ( Application
                        (Lambda (L.VarIdent "x") $ Id (L.VarIdent "x"))
                        (Id $ L.VarIdent "y")
                    )
        it "chained applications are left associative" $ do
            L.execParser expression "(\\x.x) y z"
                `shouldBe` Right
                    ( Application
                        ( Application
                            (Lambda (L.VarIdent "x") $ Id (L.VarIdent "x"))
                            (Id $ L.VarIdent "y")
                        )
                        (Id $ L.VarIdent "z")
                    )
        it "simple if-then-else expressions can be parsed" $
            do
                L.execParser expression "if 0bool then x else y"
                `shouldBe` Right
                    ( IfThenElse
                        (Lit $ L.Literal "0" ABool)
                        (Id $ L.VarIdent "x")
                        (Id $ L.VarIdent "y")
                    )
        it "nested if-then-else expressions nested after then can be parsed" $
            do
                L.execParser expression "if 0bool then if 1bool then z else x else y"
                `shouldBe` Right
                    ( IfThenElse
                        (Lit $ L.Literal "0" ABool)
                        ( IfThenElse
                            (Lit $ L.Literal "1" ABool)
                            (Id $ L.VarIdent "z")
                            (Id $ L.VarIdent "x")
                        )
                        (Id $ L.VarIdent "y")
                    )
        it "nested if-then-else expressions nested after else can be parsed" $
            do
                L.execParser expression "if 0bool then y else if 1bool then z else x"
                `shouldBe` Right
                    ( IfThenElse
                        (Lit $ L.Literal "0" ABool)
                        (Id $ L.VarIdent "y")
                        ( IfThenElse
                            (Lit $ L.Literal "1" ABool)
                            (Id $ L.VarIdent "z")
                            (Id $ L.VarIdent "x")
                        )
                    )
        it "expressions with correct linebreak can be parsed" $
            do
                L.execParser expression "if 0bool\n then x else y"
                `shouldBe` Right
                    ( IfThenElse
                        (Lit $ L.Literal "0" ABool)
                        (Id $ L.VarIdent "x")
                        (Id $ L.VarIdent "y")
                    )
        it "expressions with incorrect linebreak can't be parsed" $
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
