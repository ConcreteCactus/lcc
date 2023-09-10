import qualified LambdaCompilerTests.SemanticAnalyzer.DependencyGraphTest as DGTest
import qualified LambdaCompilerTests.SemanticAnalyzerTests as SmATest
import Test.Hspec

main :: IO ()
main = do
  hspec $ do
    DGTest.spec
    SmATest.spec
