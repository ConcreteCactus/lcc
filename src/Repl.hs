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
  (progE, env) <- loadStep file
  case progE of
    Left err -> putStrLn $ "Couldn't load file: " ++ file ++ "\n" ++ show err ++ "\n"
    Right prog -> do
      print prog
      replAgain prog
  where
    replAgain :: SemProgram -> IO ()
    replAgain prog = do
      line <- readStep
      case commandStep line of
        Just Exit -> return ()
        Nothing -> do
          let evaluated = evalStep prog line
          printStep $ evalStep prog line
          replAgain prog

loadStep :: FilePath -> IO (Either CompilerError SemProgram, Env)
loadStep path = do
    

readStep :: IO String
readStep = do
  putStr "> "
  hFlush stdout
  getLine

commandStep :: String -> Maybe Command
commandStep ":q" = Just Exit
commandStep _ = Nothing

evalStep :: SemProgram -> String -> Either (Either CompilerError RuntimeError) EvaluatedExpression
evalStep prog expS = do
  syntactic <- sinkL $ parseExpression expS
  semantic <- sinkL $ createSemanticExpression syntactic
  sinkR $ evaluateSafe prog 1000 semantic

printStep :: Either (Either CompilerError RuntimeError) EvaluatedExpression -> IO ()
printStep eval = putStrLn (show eval ++ "\n")
