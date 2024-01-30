module LambdaCompilerTests.Compiler.CompilerTests (spec) where

import Compiler.Internal
import Control.Concurrent
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
      isCompilableByGCC program1 `shouldReturn` Nothing
    it "can compile without warning" $ do
      isCompilableByGCCWoWarning program1 `shouldReturn` Nothing
    it "can compile with application" $ do
      isCompilableByGCCWoWarning program2 `shouldReturn` Nothing
    it "can compile with main fn" $ do
      isCompilableByGCCWoWarning program3 `shouldReturn` Nothing
    it "can compile with main fn and call to stdlib" $ do
      isCompilableByGCCWoWarning program4 `shouldReturn` Nothing
    it "recursive program output should be 2222" $ do
      outputOf program5 `shouldReturn` Right "2222"
    it "adt program output should be 9998" $ do
      outputOf program6 `shouldReturn` Right "9998"

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

program4 :: SourceCode
program4 =
  "one := 1i32\n"
    ++ "main := add_i32 1i32 one"

program5 :: SourceCode
program5 =
  "compose : (b -> c) -> (a -> b) -> (a -> c)\n"
    ++ "compose := \\f.\\g.\\x. f (g x)\n"
    ++ "\n"
    ++ "doUntil3 : I32 -> (a -> a) -> (a -> a)\n"
    ++ "doUntil3 := \\n.\\f. if (iseq_i32 n 3i32) then f else (compose (doUntil3 (add_i32 n 1i32) f) f)\n"
    ++ "\n"
    ++ "main : I8\n"
    ++ "main := doUntil3 0i32 (print_i32 2i32) 0i8\n"

program6 :: SourceCode
program6 =
  "main := case sumtuple (\\n. print_i32 n) (\\nm. compose (print_i16 (fst nm)) (print_i8 (snd nm))) 0i8\n" ++
  "\n" ++
  "tup : I32 * I16\n" ++
  "tup := tuple 5i32 2i16\n" ++
  "\n" ++
  "sum1 : I32 + I16\n" ++
  "sum1 := inl 12i32\n" ++
  "\n" ++
  "sum2 : I32 + I16\n" ++
  "sum2 := inr 16i16\n" ++
  "\n" ++
  "sumtuple : I32 + I16 * I8\n" ++
  "sumtuple := inr (tuple 99i16 98i8)\n" ++
  "\n" ++
  "compose := \\f.\\g.\\x. f (g x)\n"

testCompile :: SourceCode -> Either CompilerError CCode
testCompile sc = do
  scy <- leftMap mkCompErrLex $ Y.parseProgram sc
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
            let cp = (shell $ "gcc -Wall -o " ++ outfile ++ " " ++ srcPath){cwd = Just dirPath}
            readCreateProcessWithExitCode cp ""
        )

gccCompileAndRun ::
  SourceCode ->
  Either
    CompilerError
    (IO ((ExitCode, String, String), Maybe (ExitCode, String, String)))
gccCompileAndRun src = do
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
            let cp =
                  ( shell $ "gcc -Wall -o " ++ outfile ++ " " ++ srcPath
                  )
                    { cwd = Just dirPath
                    }
            (compCode, compOut, compErr) <- readCreateProcessWithExitCode cp ""
            case compCode of
              ExitSuccess | null compErr -> do
                let rcp = (shell outfile){cwd = Just dirPath}
                (runCode, runOut, runErr) <- readCreateProcessWithExitCode rcp ""
                return ((compCode, compOut, compErr), Just (runCode, runOut, runErr))
              _ -> return ((compCode, compOut, compErr), Nothing)
        )

isCompilableByGCC ::
  SourceCode ->
  IO (Maybe (Either CompilerError (ExitCode, String, String)))
isCompilableByGCC src = do
  let compilationE = gccCompile src
  case compilationE of
    Left e -> return $ Just $ Left e
    Right compilation -> do
      (code, out, err) <- compilation
      case code of
        ExitSuccess -> return Nothing
        _ -> do
          putStrLn $ "\n" ++ err
          return $ Just $ Right (code, out, err)

isCompilableByGCCWoWarning ::
  SourceCode ->
  IO (Maybe (Either CompilerError (ExitCode, String, String)))
isCompilableByGCCWoWarning src = do
  let compilationE = gccCompile src
  case compilationE of
    Left e -> return $ Just $ Left e
    Right compilation -> do
      (code, out, err) <- compilation
      case code of
        ExitSuccess | null err -> return Nothing
        _ -> do
          putStrLn $ "\n" ++ err
          return $ Just $ Right (code, out, err)

outputOf ::
  SourceCode ->
  IO (Either (Either CompilerError (ExitCode, String, String)) String)
outputOf src = do
  let compilationE = gccCompileAndRun src
  case compilationE of
    Left e -> return $ Left $ Left e
    Right compilation -> do
      ((compCode, compOut, compErr), runM) <- compilation
      case (compCode, runM) of
        (ExitSuccess, Just (ExitSuccess, output, err))
          | null compErr && null err -> return $ Right output
        _ -> do
          putStrLn $ "\n" ++ compErr
          return $ Left $ Right (compCode, compOut, compErr)

