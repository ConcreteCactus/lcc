{-# LANGUAGE TupleSections #-}

module Repl (repl) where

import Errors
import Interpreter
import SemanticAnalyzer
import SyntacticAnalyzer
import System.IO
import Util

data Command = Exit

repl :: FilePath -> IO ()
repl file = do
  loadResult <- loadStep file
  case loadResult of
    Left err -> putStrLn $ "Couldn't load file: " ++ file ++ "\n" ++ show err ++ "\n"
    Right (env, prog) -> do
      replAgain prog env
  where
    replAgain :: SemProgram -> Env -> IO ()
    replAgain prog env = do
      line <- readStep
      case commandStep line of
        Just Exit -> return ()
        Nothing -> do
          let evalResult = evalStep prog env line
          case evalResult of
            Left e -> do
              printStep $ Left e
              replAgain prog env
            Right (newEnv, evaluated) -> do
              printStep $ Right evaluated
              replAgain prog newEnv

loadStep :: FilePath -> IO (Either CompilerError (Env, SemProgram))
loadStep path = do
  progS <- readFile path
  let progE = parseProgramSingleError progS
  case progE of
    Left e -> return $ Left e
    Right prog -> do
      let (env, loaded) = runState (createSemanticProgramS prog) startingEnv
      return $ (env,) <$> loaded

readStep :: IO String
readStep = do
  putStr "> "
  hFlush stdout
  getLine

commandStep :: String -> Maybe Command
commandStep ":q" = Just Exit
commandStep _ = Nothing

evalStep ::
  SemProgram ->
  Env ->
  String ->
  Either (Either CompilerError RuntimeError) (Env, SemExpression)
evalStep prog env expS = do
  syntactic <- sinkL $ parseExpression expS
  let (newEnv, semanticE) = runState (createSemanticExpressionS syntactic) env
  semantic <- sinkL semanticE
  evaluated <- sinkR $ replaceReferences prog [] 1000 semantic >>= evaluateSafeDeep 1000
  return (newEnv, evaluated)

printStep :: Either (Either CompilerError RuntimeError) SemExpression -> IO ()
printStep (Left e) = putStrLn (show e ++ "\n")
printStep (Right a) = putStrLn (show a ++ "\n")
