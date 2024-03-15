module Main (main) where

import CodeGenerator
import Control.Monad
import Data.Maybe
import System.Environment
import System.Exit
import System.Process

data Options = Options
    { oShowHelp :: Bool
    , oShowVersion :: Bool
    , oDontRunCC :: Bool
    , oExeName :: Maybe String
    , oCSrcName :: Maybe String
    , oInputName :: String
    , oCCCmd :: Maybe String
    }

data OptionError
    = OeInputFileNotGiven
    | OeOptionArgumentNotGiven OptionArgument
    | OeMultipleOptionArgumentsSpecified OptionArgument
    | OeMultipleInputFileNamesSpecified
    | OeNoArgumentsGiven

data OptionArgument = OaExeName | OaCSrcName | OaCCName

instance Show OptionError where
    show OeInputFileNotGiven = "Error: the input file was not given"
    show OeMultipleInputFileNamesSpecified =
        "Error: multiple input file names specified"
    show (OeMultipleOptionArgumentsSpecified OaCSrcName) =
        "Error: multiple C source names specified"
    show (OeMultipleOptionArgumentsSpecified OaExeName) =
        "Error: multiple executable names specified"
    show (OeMultipleOptionArgumentsSpecified OaCCName) =
        "Error: the C compiler command was specified multiple times"
    show (OeOptionArgumentNotGiven OaExeName) =
        "Error: please specify the name of the executable "
            ++ "or don't use the -o flag"
    show (OeOptionArgumentNotGiven OaCSrcName) =
        "Error: please specify the name of the C source "
            ++ "or don't use the -O flag"
    show (OeOptionArgumentNotGiven OaCCName) =
        "Error: please specify the C compile command "
            ++ "or don't use the -c flag"
    show OeNoArgumentsGiven =
        "Error: please specify at least the name of the input file"

emptyOptions :: Options
emptyOptions =
    Options
        { oShowHelp = False
        , oShowVersion = False
        , oDontRunCC = False
        , oExeName = Nothing
        , oCSrcName = Nothing
        , oInputName = ""
        , oCCCmd = Nothing
        }

main :: IO ()
main = do
    optsE <- parseOptions <$> getArgs
    case optsE of
        Left e -> do
            print e
            putStrLn helpText
            exitWith (ExitFailure 1)
        Right opts
            | oShowHelp opts -> do
                putStr helpText
            | oShowVersion opts -> do
                putStr versionText
            | otherwise -> do
                compile opts
                unless (oDontRunCC opts) $ run opts

compile :: Options -> IO ()
compile opts = do
    src <- readFile (oInputName opts)
    let compE = compileFull src
    case compE of
        Left e -> do
            print e
            exitWith (ExitFailure 1)
        Right comp -> do
            _ <- writeFile (fromMaybe "a.c" (oCSrcName opts)) comp
            return ()

run :: Options -> IO ()
run opts = do
    let cmd = fromMaybe "gcc -g" (oCCCmd opts)
    let output = maybe "" ("-o " ++) (oExeName opts)
    let input = fromMaybe "a.c" (oCSrcName opts)
    callCommand $ cmd ++ " " ++ output ++ " " ++ input

parseOptions :: [String] -> Either OptionError Options
parseOptions [] = Left OeNoArgumentsGiven
parseOptions args =
    checkForErrors
        =<< foldl foldOpt (Right (emptyOptions, Nothing)) args
  where
    checkForErrors ::
        (Options, Maybe OptionArgument) ->
        Either OptionError Options
    checkForErrors (_, Just oa) = Left (OeOptionArgumentNotGiven oa)
    checkForErrors (opts, Nothing)
        | null (oInputName opts)
            && not (oShowHelp opts)
            && not (oShowVersion opts) =
            Left OeInputFileNotGiven
        | otherwise = Right opts
    foldOpt ::
        Either OptionError (Options, Maybe OptionArgument) ->
        String ->
        Either OptionError (Options, Maybe OptionArgument)
    foldOpt (Left e) _ = Left e
    foldOpt (Right (opts, Just optarg)) arg = Right (optargF optarg arg opts, Nothing)
    foldOpt (Right (opts, Nothing)) "-h" = Right (opts{oShowHelp = True}, Nothing)
    foldOpt (Right (opts, Nothing)) "-v" = Right (opts{oShowVersion = True}, Nothing)
    foldOpt (Right (opts, Nothing)) "-n" = Right (opts{oDontRunCC = True}, Nothing)
    foldOpt (Right (opts, Nothing)) "-o"
        | isNothing (oExeName opts) = Right (opts, Just OaExeName)
        | otherwise = Left $ OeMultipleOptionArgumentsSpecified OaExeName
    foldOpt (Right (opts, Nothing)) "-O"
        | isNothing (oCSrcName opts) = Right (opts, Just OaCSrcName)
        | otherwise = Left $ OeMultipleOptionArgumentsSpecified OaCSrcName
    foldOpt (Right (opts, Nothing)) "-c"
        | isNothing (oCCCmd opts) = Right (opts, Just OaCCName)
        | otherwise = Left $ OeMultipleOptionArgumentsSpecified OaCCName
    foldOpt (Right (opts, Nothing)) arg
        | null (oInputName opts) = Right (opts{oInputName = arg}, Nothing)
        | otherwise = Left OeMultipleInputFileNamesSpecified
    optargF :: OptionArgument -> String -> Options -> Options
    optargF OaExeName arg opts = opts{oExeName = Just arg}
    optargF OaCSrcName arg opts = opts{oCSrcName = Just arg}
    optargF OaCCName arg opts = opts{oCCCmd = Just arg}

versionText :: String
versionText =
    unlines
        [ "lambda_compiler v1.0"
        , ""
        , "___"
        , "\\_ \\"
        , "  \\ \\       __     __       __     __"
        , "   \\ \\     (_ \\   / _)     (_ \\   / _)"
        , "   /  \\      \\ \\_/ /         \\ \\_/ /"
        , "  / /\\ \\      ) _ (           ) _ ("
        , " / /  \\ \\   _/ / \\ \\_       _/ / \\ \\_"
        , "/_/    \\_\\ (__/   \\__) (_) (__/   \\__)"
        , ""
        , "This project is part of the undergraduate thesis of Áron Hárnási"
        , "from Eötvös Lóránd University."
        ]

helpText :: String
helpText =
    unlines
        [ "Usage: lcc [options] <sourcecode.lc>"
        , ""
        , "This program will compile a .lc source file into a .c source file"
        , "which will then be fed into gcc or a different C compiler if"
        , "specified."
        , ""
        , "Valid options are:"
        , "\t-n \t\t Do not run any C compiler, only compile from .lc to .c"
        , "\t-h \t\t Print this help and exit."
        , "\t-v \t\t Show version and about information and exit."
        , "\t-o <output-exe>\t Specify the name of the executable generated"
        , "\t-O <output.c>\t Specify the name of the .c file generated"
        , "\t-c <cc>\t\t Specify C compiler command"
        ]
