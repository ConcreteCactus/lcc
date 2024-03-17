module LambdaCompilerTests.SemanticAnalyzerTests (spec) where

import Errors
import AtomicType
import qualified Lexer as L
import SemanticAnalyzer.Expression
import SemanticAnalyzer.Internal
import SemanticAnalyzer.Type
import qualified SyntacticAnalyzer.Internal as Y
import Test.Hspec
import Util

spec :: Spec
spec = do
  describe "convertExpression" $ do
    it "can convert simple expressions" $ do
      test "1i32" `shouldBe` Right (Lit (L.Literal "1" AI32))
      test "\\a.a" `shouldBe` Right (lam "a" (Ident 1))
      test "\\a.\\b.a b"
        `shouldBe` Right (lam "a" (lam "b" (appl (Ident 2) (Ident 1))))
      test "(\\a.a) x"
        `shouldBe` Right (appl (lam "a" (Ident 1)) (Ref (L.VarIdent "x")))
      test "if 1bool then \\a.\\b.a else \\a.\\b.b"
        `shouldBe` Right
          ( IfThenElse
              (Lit (L.Literal "1" ABool))
              (lam "a" $ lam "b" $ Ident 2)
              (lam "a" $ lam "b" $ Ident 1)
          )
  describe "convertType" $ do
    it "can convert simple types" $ do
      testT "a" `shouldBe` Right (mkn (GenericType 1))
      testT "a -> a"
        `shouldBe` Right (mkn $ funt (GenericType 1) (GenericType 1))
      testT "(a -> a) -> a"
        `shouldBe` Right
          (mkn $ funt (funt (GenericType 1) (GenericType 1)) (GenericType 1))
  describe "Program" $ do
    it "can parse a simple program" $ do
      isRight (testP program1) `shouldBe` True
      isRight (testP program2) `shouldBe` True
      isRight (testP program3) `shouldBe` False
      isRight (testP program4) `shouldBe` True
      isRight (testP program5) `shouldBe` True
      isRight (testP program6) `shouldBe` True
    it "can infer simple types correctly" $ do
      teType . defExpr . head . progDefs <$> testP program1
        `shouldBe` Right
          ( mkn $ funt (GenericType 1) (funt (GenericType 2) (GenericType 1))
          )
      teType . defExpr . head . tail . progDefs <$> testP program1
        `shouldBe` Right
          ( mkn $ funt (GenericType 1) (funt (GenericType 2) (GenericType 2))
          )
    it "can work with type wishes correctly" $ do
      teType . defExpr . head . progDefs <$> testP program6
        `shouldBe` Right
          (mkn $ funt (GenericType 1) (funt (GenericType 1) (GenericType 1)))
      teType . defExpr . head . tail . progDefs <$> testP program6
        `shouldBe` Right
          (mkn $ funt (GenericType 1) (funt (GenericType 1) (GenericType 1)))
    it "can work with dependency cycles" $ do
      map (teType . defExpr) . progDefs <$> testP program7
        `shouldBe` Right
          [ mkn $ funt (GenericType 1) (GenericType 2)
          , mkn $ funt (GenericType 1) (GenericType 2)
          , mkn $ funt (GenericType 1) (GenericType 2)
          ]
    it "can reconcile recursive variables with themselves" $ do
      isRight (testP program8)
        `shouldBe` False
      map (teType . defExpr) . progDefs <$> testP program9
        `shouldBe` Right [mkn $ funt (GenericType 1) (GenericType 2)]
    it "can work with recursive definitions and dependency cycles" $ do
      map (teType . defExpr) . progDefs <$> testP program10
        `shouldBe` Right
          [ mkn $ funt (GenericType 1) (GenericType 1)
          , mkn $ funt (GenericType 1) (funt (GenericType 1) (GenericType 1))
          ]
    it
      ( "can work with recursive definitions and dependency cycles"
          ++ " with a type wish"
      )
      $ do
        map (teType . defExpr) . progDefs <$> testP program11
          `shouldBe` Right
            [ mkn $ funt (GenericType 1) (GenericType 1)
            , mkn $ funt (GenericType 1) (funt (GenericType 1) (GenericType 1))
            ]
    it
      ( "can work with recursive definitions and dependency cycles"
          ++ " with a different type wish"
      )
      $ do
        map (teType . defExpr) . progDefs <$> testP program12
          `shouldBe` Right
            [ mkn $ funt nai nai
            , mkn $ funt nai (funt nai nai)
            ]
    it "can adapt to type wishes in dependency cycles" $ do
      map (teType . defExpr) . progDefs <$> testP program13
        `shouldBe` Right
          [ mkn $ funt nai nai
          , mkn $ funt nai (funt nai nai)
          ]
    it "can work with type wishes in dependency trees" $ do
      map (teType . defExpr) . progDefs <$> testP program14
        `shouldBe` Right
          [ mkn $ funt (GenericType 1) (funt nai nai)
          , mkn $ funt nai nai
          ]

isRight :: Either a b -> Bool
isRight (Right _) = True
isRight _ = False

convertExpression :: Y.Expression -> Expression
convertExpression expr = 
  execState (convertExpressionS expr) $ ConvertEnv [] [] []

parseExpression :: L.SourceCode -> Either LexicalError Y.Expression
parseExpression = L.execParser Y.expression

parseType :: L.SourceCode -> Either LexicalError Y.Type
parseType = L.execParser Y.type_

test :: SourceCode -> Either LexicalError Expression
test s = convertExpression <$> parseExpression s

testT :: SourceCode -> Either LexicalError NormType
testT s = convertType <$> parseType s

testP :: SourceCode -> Either CompilerError Program
testP s =
  leftMap mkCompErrLex (Y.parseProgram s)
    >>= mkProgramFromSyn

nai :: Type
nai = AtomicType AI32

lam :: String -> Expression -> Expression
lam s = Lambda (L.VarIdent s)

appl :: Expression -> Expression -> Expression
appl = Application

funt :: Type -> Type -> Type
funt = FunctionType

mkn :: Type -> NormType
mkn = mkNormType

-- ident :: String -> L.Ident
-- ident = L.Ident

program1 :: SourceCode
program1 =
  "true := \\a.\\b.a\n"
    ++ "false := \\a.\\b.b"

program2 :: SourceCode
program2 =
  "true := \\a.\\b.a\n"
    ++ "false := \\a.\\b.b\n"
    ++ "and := \\a.\\b. a b false\n"
    ++ "or := \\a.\\b. a true b\n"
    ++ "not := \\a. a false true\n"
    ++ "xor := \\a.\\b. a (not b) b"

program3 :: SourceCode
program3 =
  "true := \\a.\\b.a\n"
    ++ "false := \\a.\\b.b\n"
    ++ "and := \\a.\\b. a b false\n"
    ++ "or := \\a.\\b. a true b\n"
    ++ "not := \\a. a false true\n"
    ++ "xor := \\a.\\b. a (notE b) b"

program4 :: SourceCode
program4 =
  "te := \\a. \\b. a b\n"

program5 :: SourceCode
program5 =
  "zero := \\f. \\z. z\n"
    ++ "succ := \\n. \\f. \\z. f (n f z)\n"
    ++ "one := succ zero\n"
    ++ "two := succ one\n"
    ++ "three := succ two\n"

program6 :: SourceCode
program6 =
  "true : a -> a -> a\n"
    ++ "true := \\a.\\b.a\n"
    ++ "false : a -> a -> a\n"
    ++ "false := \\a.\\b.b\n"

program7 :: SourceCode
program7 =
  "a := \\x.c x\n"
    ++ "b := \\x.a x\n"
    ++ "c := \\x.b x\n"

program8 :: SourceCode
program8 =
  "a := \\x. a"

program9 :: SourceCode
program9 =
  "a := \\x. a x"

program10 :: SourceCode
program10 =
  "a := \\x.b x x\n"
    ++ "b := \\x.\\y.a (b x y)\n"

program11 :: SourceCode
program11 =
  "a : a -> a \n"
    ++ "a := \\x.b x x\n"
    ++ "b : a -> a -> a\n"
    ++ "b := \\x.\\y.a (b x y)\n"

program12 :: SourceCode
program12 =
  "a : I32 -> I32 \n"
    ++ "a := \\x.b x x\n"
    ++ "b : I32 -> I32 -> I32\n"
    ++ "b := \\x.\\y.a (b x y)\n"

program13 :: SourceCode
program13 =
  "a : I32 -> I32 \n"
    ++ "a := \\x.b x x\n"
    ++ "b := \\x.\\y.a (b x y)\n"

program14 :: SourceCode
program14 =
  "a : I32 -> I32 \n"
    ++ "a := \\x. x\n"
    ++ "b := \\x.\\y.a y\n"
