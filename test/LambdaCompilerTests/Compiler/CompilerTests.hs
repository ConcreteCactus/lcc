module LambdaCompilerTests.Compiler.CompilerTests (spec) where

import Compiler.Internal
import Errors
import SemanticAnalyzer as S
import SyntacticAnalyzer as Y
import System.Exit
import System.IO
import System.IO.Temp
import System.Process
import Test.Hspec
import Util

spec :: Spec
spec = do
  describe "Compiler (system tests)" $ do
    it "can compile" $ do
      isCompilableByGCC program1 `shouldReturn` True
    it "can compile without warning" $ do
      isCompilableByGCCWoWarning program1 `shouldReturn` True
    it "can compile with application" $ do
      isCompilableByGCCWoWarning program2 `shouldReturn` True
    it "can compile with main fn" $ do
      isCompilableByGCCWoWarning program3 `shouldReturn` True

program1 :: SourceCode
program1 =
  "true := \\a.\\b.a\n"
    ++ "false := \\a.\\b.b"

program2 :: SourceCode
program2 =
  "true := \\a.\\b.a\n"
    ++ "false := \\a.\\b.b\n"
    ++ "and := \\a.\\b.a b false"

program3 :: SourceCode
program3 =
  "true := \\a.\\b.a\n"
    ++ "false := \\a.\\b.b\n"
    ++ "and := \\a.\\b.a b false\n"
    ++ "main := and true false\n"

testCompile :: SourceCode -> Either CompilerError CCode
testCompile sc = do
  scy <- leftMap mkCompErrLex $ Y.parseProgramSingleError sc
  scs <- S.mkProgramFromSyn scy
  return $ compile scs

gccCompile ::
  SourceCode ->
  Either CompilerError (IO (ExitCode, String, String))
gccCompile src = do
  let csrcE = testCompile src
  case csrcE of
    Left e -> Left e
    Right csrc -> Right $ do
      withSystemTempDirectory
        "lctmp"
        ( \dirPath -> do
            (srcPath, srcHandle) <- openTempFile dirPath "src.c"
            hPutStr srcHandle csrc
            hClose srcHandle
            let outfile = dirPath ++ "/a.out"
            let cp = shell $ "gcc -Wall -o " ++ outfile ++ " " ++ srcPath
            readCreateProcessWithExitCode cp ""
        )

isCompilableByGCC :: SourceCode -> IO Bool
isCompilableByGCC src = do
  let compilationE = gccCompile src
  case compilationE of
    Left _ -> return False
    Right compilation -> do
      (code, _, err) <- compilation
      case code of
        ExitSuccess -> return True
        _ -> do
          putStrLn $ "\n" ++ err
          return False

isCompilableByGCCWoWarning :: SourceCode -> IO Bool
isCompilableByGCCWoWarning src = do
  let compilationE = gccCompile src
  case compilationE of
    Left _ -> return False
    Right compilation -> do
      (code, _, err) <- compilation
      case code of
        ExitSuccess | null err -> return True
        _ -> do
          putStrLn $ "\n" ++ err
          return False
