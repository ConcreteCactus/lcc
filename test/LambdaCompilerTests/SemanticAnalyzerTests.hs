module LambdaCompilerTests.SemanticAnalyzerTests (spec) where

import Errors
import qualified Lexer as L
import SemanticAnalyzer.Expression
import SemanticAnalyzer.Internal
import SemanticAnalyzer.Type
import qualified SyntacticAnalyzer as Y
import Test.Hspec

spec :: Spec
spec = do
  describe "convertExpression" $ do
    it "can convert simple expressions" $ do
      test "\\a.a" `shouldBe` Right (lam "a" (Ident 1))
      test "\\a.\\b.a b" `shouldBe` Right (lam "a" (lam "b" (appl (Ident 2) (Ident 1))))
      test "(\\a.a) x" `shouldBe` Right (appl (lam "a" (Ident 1)) (Ref (L.Ident "x")))
  describe "convertType" $ do
    it "can convert simple types" $ do
      testT "a" `shouldBe` Right (mkn (GenericType 1))
      testT "a -> a" `shouldBe` Right (mkn $ funt (GenericType 1) (GenericType 1))
      testT "(a -> a) -> a" `shouldBe` Right (mkn $ funt (funt (GenericType 1) (GenericType 1)) (GenericType 1))
  describe "Program" $ do
    it "can parse a simple program" $ do
      isRight (testP program1) `shouldBe` True
      isRight (testP program2) `shouldBe` True
      isRight (testP program3) `shouldBe` False
      isRight (testP program4) `shouldBe` True
      isRight (testP program5) `shouldBe` True
      isRight (testP program6) `shouldBe` True

isRight :: Either a b -> Bool
isRight (Right _) = True
isRight _ = False

test :: SourceCode -> Either CompilerError Expression
test s = convertExpression <$> Y.parseExpression s

testT :: SourceCode -> Either CompilerError NormType
testT s = convertType <$> Y.parseType s

testP :: SourceCode -> Either CompilerError Program
testP s = Y.parseProgramSingleError s >>= mkProgramFromSyn

nai :: Type
nai = AtomicType AInt

lam :: String -> Expression -> Expression
lam s = Lambda (L.Ident s)

appl :: Expression -> Expression -> Expression
appl = Application

funt :: Type -> Type -> Type
funt = FunctionType

mkn :: Type -> NormType
mkn = mkNormType

ident :: String -> L.Ident
ident = L.Ident

-- defi :: String -> Expression -> Definition
-- defi s = Definition (ident s)

semanticAnalyzerTests :: [Bool]
semanticAnalyzerTests =
  [ test "\\a.a" == Right (lam "a" (Ident 1)),
    test "\\a.\\b.a b" == Right (lam "a" (lam "b" (Application (Ident 2) (Ident 1)))),
    test "(\\a.a) x" == Left (SemanticError $ SUndefinedVariable "x"),
    testT "int" == Right (mkn nai),
    testT "int -> int" == Right (mkn $ funt nai nai),
    testT "(int -> int) -> int" == Right (mkn $ funt (funt nai nai) nai)
    -- testP program1 == Right program1ShouldBe,
    -- testP program2 == Right program2ShouldBe,
    -- testP program3 == Left (SemanticError $ SUndefinedVariable "notE")
    -- parseAndCreateProgram program1 == Right program1ShouldBe,
    -- parseAndCreateProgram program2 == Right program2ShouldBe,
    -- (parseAndCreateProgram program1 >>= (`parseAndCreateExpressionWithProgram` "true")) == Right (Ref $ ident "true"),
    -- parseAndCreateProgram program4 == Right program4ShouldBe,
    -- parseAndCreateProgram program5 == Right program5ShouldBe,
    -- parseAndCreateProgram program6 == Right program6ShouldBe
  ]

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

--
-- program1ShouldBe :: Program
-- program1ShouldBe =
--   Program
--     [ident "true", ident "false"]
--     [ Definition "true" (lam "a" (lam "b" (Ident 2))) (mkn $ funt (GenericType 1) (funt (GenericType 2) (GenericType 1))),
--       Definition "false" (lam "a" (lam "b" (Ident 1))) (mkn $ funt (GenericType 1) (funt (GenericType 2) (GenericType 2)))
--     ]
--
--
-- program2ShouldBe :: Program
-- program2ShouldBe =
--   Program
--     [ident "true", ident "false", ident "and", ident "or", ident "not", ident "xor"]
--     [ Definition (ident "true") (lam "a" (lam "b" (Ident 2))) (funt (GenericType 1) (funt (GenericType 2) (GenericType 1))),
--       Definition "false" (lam "a" (lam "b" (Ident 1))) (funt (GenericType 1) (funt (GenericType 2) (GenericType 2))),
--       Definition "and" (lam "a" (lam "b" (Application (Application (Ident 2) (Ident 1)) (Ref "false")))) (funt (funt (GenericType 1) (funt (funt (GenericType 2) (funt (GenericType 3) (GenericType 3))) (GenericType 4))) (funt (GenericType 1) (GenericType 4))),
--       Definition "or" (lam "a" (lam "b" (Application (Application (Ident 2) (Ref "true")) (Ident 1)))) (funt (funt (funt (GenericType 1) (funt (GenericType 2) (GenericType 1))) (funt (GenericType 3) (GenericType 4))) (funt (GenericType 3) (GenericType 4))),
--       Definition "not" (lam "a" (Application (Application (Ident 1) (Ref "false")) (Ref "true"))) (funt (funt (funt (GenericType 1) (funt (GenericType 2) (GenericType 2))) (funt (funt (GenericType 3) (funt (GenericType 4) (GenericType 3))) (GenericType 5))) (GenericType 5)),
--       Definition "xor" (lam "a" (lam "b" (Application (Application (Ident 2) (Application (Ref "not") (Ident 1))) (Ident 1)))) (funt (funt (GenericType 1) (funt (funt (funt (GenericType 2) (funt (GenericType 3) (GenericType 3))) (funt (funt (GenericType 4) (funt (GenericType 5) (GenericType 4))) (GenericType 1))) (GenericType 6))) (funt (funt (funt (GenericType 2) (funt (GenericType 3) (GenericType 3))) (funt (funt (GenericType 4) (funt (GenericType 5) (GenericType 4))) (GenericType 1))) (GenericType 6)))
--     ]
--
-- program4ShouldBe :: Program
-- program4ShouldBe =
--   Program
--     ["te"]
--     [Definition "te" (lam "a" (lam "b" (Application (Ident 2) (Ident 1)))) (funt (funt (GenericType 1) (GenericType 2)) (funt (GenericType 1) (GenericType 2)))]
--
-- program5ShouldBe :: Program
-- program5ShouldBe =
--   Program
--     ["zero", "succ", "one", "two", "three"]
--     [ Definition "zero" (lam "f" (lam "z" (Ident 1))) (funt (GenericType 1) (funt (GenericType 2) (GenericType 2))),
--       Definition "succ" (lam "n" (lam "f" (lam "z" (Application (Ident 2) (Application (Application (Ident 3) (Ident 2)) (Ident 1)))))) (funt (funt (funt (GenericType 1) (GenericType 2)) (funt (GenericType 3) (GenericType 1))) (funt (funt (GenericType 1) (GenericType 2)) (funt (GenericType 3) (GenericType 2)))),
--       Definition "one" (Application (Ref "succ") (Ref "zero")) (funt (funt (GenericType 1) (GenericType 2)) (funt (GenericType 1) (GenericType 2))),
--       Definition "two" (Application (Ref "succ") (Ref "one")) (funt (funt (GenericType 1) (GenericType 1)) (funt (GenericType 1) (GenericType 1))),
--       Definition "three" (Application (Ref "succ") (Ref "two")) (funt (funt (GenericType 1) (GenericType 1)) (funt (GenericType 1) (GenericType 1)))
--     ]
--
-- program6ShouldBe :: Program
-- program6ShouldBe =
--   Program
--     ["true", "false"]
--     [ Definition "true" (lam "a" (lam "b" (Ident 2))) (funt (GenericType 1) (funt (GenericType 1) (GenericType 1))),
--       Definition "false" (lam "a" (lam "b" (Ident 1))) (funt (GenericType 1) (funt (GenericType 1) (GenericType 1)))
--     ]
--
