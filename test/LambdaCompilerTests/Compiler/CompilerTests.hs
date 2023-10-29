module LambdaCompilerTests.Compiler.CompilerTests (spec) where

import Compiler.Internal
import Errors
import SemanticAnalyzer as S
import SyntacticAnalyzer as Y
import System.Process
import System.Exit
import Test.Hspec
import System.IO.Temp
import System.IO
import Debug.Trace

spec :: Spec
spec = do
  describe "Compiler" $ do
    it "can compile" $ do
      isCompilableByGCC program1 `shouldReturn` True
program1 :: SourceCode
program1 =
  "true := \\a.\\b.a\n"
    ++ "false := \\a.\\b.b"

testCompile :: SourceCode -> Either CompilerError CCode
testCompile sc = do
  scy <- Y.parseProgramSingleError sc
  scs <- S.mkProgramFromSyn scy
  return $ compile scs

isCompilableByGCC :: SourceCode -> IO Bool
isCompilableByGCC src = do
  let csrcE = testCompile src
  case csrcE of
    Left _ -> return False
    Right csrc -> trace csrc $ do
      withSystemTempDirectory "lctmp" (\dirPath -> do
        (srcPath, srcHandle) <- openTempFile dirPath "src.c"
        hPutStr srcHandle csrc
        let cp = shell $ "gcc -Wall " ++ srcPath
        (code, _, err) <- readCreateProcessWithExitCode cp ""
        case code of
          ExitSuccess -> trace err $ return True
          _ -> return False
        )
