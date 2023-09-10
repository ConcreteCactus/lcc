import qualified LambdaCompilerTests.SemanticAnalyzer.DependencyGraphTest as DGTest
import qualified LambdaCompilerTests.SemanticAnalyzer.TypeTest as TyTest
import qualified LambdaCompilerTests.SemanticAnalyzerTests as SmATest
import Test.Hspec

main :: IO ()
main = do
  hspec $ do
    DGTest.spec
    TyTest.spec
    SmATest.spec
