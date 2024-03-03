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

data TestResult
    = TrCompErr CompilerError
    | TrGccErr (ExitCode, String, String)
    | TrRuntimeErr (ExitCode, String, String)
    | TrValgrindErr (ExitCode, String, String)
    | TrSuccess String
    deriving (Show, Eq)

data RunResult
    = RrCompErr CompilerError
    | RrGccErr (ExitCode, String, String)
    | RrValgrindErr (ExitCode, String, String)
    | RrRuntimeOutput (ExitCode, String, String)
    deriving (Show, Eq)

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
            outputOf program5 `shouldReturn` TrSuccess "2222"
        it "adt program output should be 98100" $ do
            outputOf program6 `shouldReturn` TrSuccess "98100"
        it "casting program should output 18" $ do
            outputOf program7 `shouldReturn` TrSuccess "20.300000"
        it "literal printing program should output Ac-12B" $ do
            outputOf program8 `shouldReturn` TrSuccess "Ac-12B"
        it "list printing program should output abc" $ do
            outputOf program9 `shouldReturn` TrSuccess "abc"
        it "value of if expressions sould be taken care of by gc" $ do
            outputOf program10 `shouldReturn` TrSuccess "-11"
        it "will compile program with unspecified types" $ do
            isCompilableByGCCWoWarning program11 `shouldReturn` Nothing
        it "won't compile program with specified types if they don't match" $ do
            hasCompileError <$> isCompilableByGCCWoWarning program12
                `shouldReturn` True
        it "logic statement works" $ do
            isCompilableByGCCWoWarning program13 `shouldReturn` Nothing

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
    "compose : (b -> returnType) -> (a -> b) -> (a -> returnType)\n"
        ++ "compose := \\f.\\g.\\x. f (g x)\n"
        ++ "\n"
        ++ "doUntil3 : I32 -> (a -> a) -> (a -> a)\n"
        ++ "doUntil3 := \\n.\\f. if (iseq_i32 n 3i32) then f else (compose (doUntil3 (add_i32 n 1i32) f) f)\n"
        ++ "\n"
        ++ "main : I8\n"
        ++ "main := doUntil3 0i32 (print_i32 2i32) 0i8\n"

program6 :: SourceCode
program6 =
    "main := case sumtuple (\\n. print_i32 n) (\\nm. compose (print_i16 (add_i16 1i16 (fst nm))) (print_i8 (snd nm))) 0i8\n"
        ++ "\n"
        ++ "tup : I32 * I16\n"
        ++ "tup := tuple 5i32 2i16\n"
        ++ "\n"
        ++ "sum1 : I32 + I16\n"
        ++ "sum1 := inl 12i32\n"
        ++ "\n"
        ++ "sum2 : I32 + I16\n"
        ++ "sum2 := inr 16i16\n"
        ++ "\n"
        ++ "sumtuple : I32 + I16 * I8\n"
        ++ "sumtuple := inr (tuple 99i16 98i8)\n"
        ++ "\n"
        ++ "compose := \\f.\\g.\\x. f (g x)\n"

program7 :: SourceCode
program7 =
    "v1 := 1i8\n"
        ++ "v2 := add_i16 1i16 (i8Toi16 v1)\n"
        ++ "v3 := add_i32 1i32 (i16Toi32 v2)\n"
        ++ "v4 := add_i64 1i64 (i32Toi64 v3)\n"
        ++ "v5 := add_i128 1i128 (i64Toi128 v4)\n"
        ++ "v6 := add_i64 1i64 (i128Toi64 v5)\n"
        ++ "v7 := add_i32 1i32 (i64Toi32 v6)\n"
        ++ "v8 := add_i16 1i16 (i32Toi16 v7)\n"
        ++ "v9 := add_i8 1i8 (i16Toi8 v8)\n"
        ++ "v10 := add_u8 1u8 (i8Tou8 v9)\n"
        ++ "v11 := add_u16 1u16 (u8Tou16 v10)\n"
        ++ "v12 := add_u32 1u32 (u16Tou32 v11)\n"
        ++ "v13 := add_u64 1u64 (u32Tou64 v12)\n"
        ++ "v14 := add_u128 1u128 (u64Tou128 v13)\n"
        ++ "v15 := add_u64 1u64 (u128Tou64 v14)\n"
        ++ "v16 := add_u32 1u32 (u64Tou32 v15)\n"
        ++ "v17 := add_u16 1u16 (u32Tou16 v16)\n"
        ++ "v18 := add_u8 1u8 (u16Tou8 v17)\n"
        ++ "v19 := add_f32 1.1f32 (u8Tof32 v18)\n"
        ++ "v20 := add_f64 1.2f64 (f32Tof64 v19)\n"
        ++ "main := print_f64 v20 0i8\n"

program8 :: SourceCode
program8 =
    "reverseCompose := \\f.\\g.\\x. g (f x)\n"
        ++ "id := \\x.x\n"
        ++ "main := reverseCompose (\n"
        ++ "    reverseCompose (\n"
        ++ "        reverseCompose (print_char 65char)\n"
        ++ "        (print_char 'c')\n"
        ++ "        ) (print_i8 -12i8)\n"
        ++ "    ) (print_char 0x42_char) 0i8\n"
program9 :: SourceCode
program9 =
    "printList : [Char] -> a -> a\n"
        ++ "printList := \\list. case (uncons list) (\\cl. rcompose (print_char (fst cl)) (printList (snd cl))) (\\e. id)\n"
        ++ "rcompose := \\f.\\g.\\x.g (f x)\n"
        ++ "id := \\x.x\n"
        ++ "main := printList (cons 'a' (cons 'b' (cons 'c' emptyList))) 0i8\n"

program10 :: SourceCode
program10 =
    "ifret := gcret (if iseq_i32 1i32 0i32 then (\\x.x) else (\\x.-11i32)) unit\n"
        ++ "gcret := \\x.\\y. tuple x y\n"
        ++ "main := print_i32 ((fst ifret) 0i32) 0i8"

program11 :: SourceCode
program11 =
    "f1 := \\x.x\n"
        ++ "main := f1 0i32"

program12 :: SourceCode
program12 =
    "f1 : I8 -> I8\n"
        ++ "f1 := \\x.x\n"
        ++ "main := f1 0i32"

program13 :: SourceCode
program13 =
    "stmt : a + Empty -> a\n" ++
    "stmt := \\ae.case ae (\\a.a) (\\e. exfalso e)"

hasCompileError ::
    Maybe (Either CompilerError (ExitCode, String, String)) ->
    Bool
hasCompileError Nothing = False
hasCompileError (Just (Right _)) = False
hasCompileError (Just (Left _)) = True

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

gccCompileAndRunWithValgrind :: SourceCode -> IO RunResult
gccCompileAndRunWithValgrind src = do
    let csrcE = testCompile src
    case csrcE of
        Left e -> return $ RrCompErr e
        Right csrc ->
            withSystemTempDirectory
                "lctmp"
                ( \dirPath -> do
                    (srcPath, srcHandle) <- openTempFile dirPath "src.c"
                    hPutStr srcHandle csrc
                    hClose srcHandle
                    let outfile = dirPath ++ "/a.out"
                    let cp =
                            (shell $ "gcc -Wall -o " ++ outfile ++ " " ++ srcPath)
                                { cwd = Just dirPath
                                }
                    (compCode, compOut, compErr) <- readCreateProcessWithExitCode cp ""
                    case compCode of
                        ExitSuccess | null compErr -> do
                            let rcp = (shell outfile){cwd = Just dirPath}
                            let vrcp =
                                    (shell $ "valgrind --error-exitcode=101 " ++ outfile)
                                        { cwd = Just dirPath
                                        }
                            (valCode, valOut, valErr) <-
                                readCreateProcessWithExitCode vrcp ""
                            (runCode, runOut, runErr) <-
                                readCreateProcessWithExitCode rcp ""
                            case valCode of
                                ExitSuccess ->
                                    return $
                                        RrRuntimeOutput (runCode, runOut, runErr)
                                _failure -> return $ RrValgrindErr (valCode, valOut, valErr)
                        _failure -> return $ RrGccErr (compCode, compOut, compErr)
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
                _failure -> do
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
                _failure -> do
                    putStrLn $ "\n" ++ err
                    return $ Just $ Right (code, out, err)

outputOf :: SourceCode -> IO TestResult
outputOf src = do
    runResult <- gccCompileAndRunWithValgrind src
    return $ case runResult of
        RrCompErr e -> TrCompErr e
        RrGccErr e -> TrGccErr e
        RrValgrindErr e -> TrValgrindErr e
        RrRuntimeOutput e@(ExitFailure _, _, _) -> TrRuntimeErr e
        RrRuntimeOutput e@(ExitSuccess, _, err) | not (null err) -> TrRuntimeErr e
        RrRuntimeOutput (ExitSuccess, o, _) -> TrSuccess o
