module LambdaCompilerTests.Compiler.CompilerTests(spec) where

import Test.Hspec
import SemanticAnalyzer as S
import SyntacticAnalyzer as Y
import Compiler.Internal
import Errors

spec :: Spec
spec = do
  describe "Compiler" $ do
    it "can compile" $ do
      testCompile program1 `shouldBe` Right ""

program1 :: SourceCode
program1 =
  "true := \\a.\\b.a\n"
    ++ "false := \\a.\\b.b"

testCompile :: SourceCode -> Either CompilerError CCode
testCompile sc = do
    scy <- Y.parseProgramSingleError sc
    scs <- S.mkProgramFromSyn scy
    return $ compile scs
