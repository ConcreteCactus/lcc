module Repl (repl) where

import Errors
import Interpreter
import SemanticAnalyzer
import System.IO
import Util
import Util.Limited

data Control = Quit

replAgain :: SemProgram -> IO ()
replAgain program = do
  readStrE <- readStep
  case readStrE of
    Left Quit -> return ()
    Right exprStr -> do
      let exprE = evalStep program exprStr
      case exprE of
        Left e -> do
          print e
          replAgain program
        Right expr -> do
          let runResult = runStep program expr
          printStep runResult
          replAgain program

repl :: String -> IO ()
repl srcPath = do
  fileStr <- readFile srcPath
  let semanticE = parseAndCreateProgram fileStr
  case semanticE of
    Left e -> print e
    Right program -> do
      replAgain program

readStep :: IO (Either Control String)
readStep = do
  putStr "λ> "
  hFlush stdout
  line <- getLine
  case trim line of
    "" -> readStep
    ":q" -> return $ Left Quit
    _ -> return $ Right line

evalStep :: SemProgram -> String -> Either CompilerError SemExpression
evalStep = parseAndCreateExpressionWithProgram

runStep :: SemProgram -> SemExpression -> Either RuntimeError SemExpression
runStep (SemProgram _ parts) expression =
  case runLimited (Limit 1000) (evaluate parts expression) of
    Nothing -> Left ComputationLimitReached
    Just a -> a

printStep :: Either RuntimeError SemExpression -> IO ()
printStep (Left e) = print e
printStep (Right a) = print a
