import qualified LambdaCompilerTests.SemanticAnalyzer.DependencyGraphTest as DGTest
import qualified LambdaCompilerTests.SemanticAnalyzer.TypeTest as TyTest
import qualified LambdaCompilerTests.SemanticAnalyzerTests as SmATest
import qualified LambdaCompilerTests.SemanticAnalyzer.DependencyListTests as DLTest
import qualified LambdaCompilerTests.Compiler.CompilerTests as CTest
import qualified LambdaCompilerTests.LexerTests as LTest
import Test.Hspec

main :: IO ()
main = do
  hspec $ do
    LTest.spec
    SmATest.spec
    DGTest.spec
    DLTest.spec
    TyTest.spec
    CTest.spec
