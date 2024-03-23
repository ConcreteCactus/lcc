import qualified LambdaCompilerTests.SemanticAnalyzer.TypeTest as TyTest
import qualified LambdaCompilerTests.SemanticAnalyzerTests as SmATest
import qualified LambdaCompilerTests.SyntacticAnalyzerTests as SyATest
import qualified LambdaCompilerTests.SemanticAnalyzer.DependencyListTests as DLTest
import qualified LambdaCompilerTests.CodeGenerator.CodeGeneratorTests as CTest
import Test.Hspec

main :: IO ()
main = do
  hspec $ do
    SyATest.spec
    SmATest.spec
    DLTest.spec
    TyTest.spec
    CTest.spec
