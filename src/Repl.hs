{-# LANGUAGE LambdaCase #-}

module Repl (repl) where

import Data.List (find)
import Data.Maybe
import Errors
import Interpreter.Alpha
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
    Right prog@(SemProgram env _) -> do
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
            Right evaluated -> do
              let replaced = replacingStep prog evaluated
              printStep $ Right replaced
              printStep $ Right evaluated
              replAgain prog env

loadStep :: FilePath -> IO (Either CompilerError SemProgram)
loadStep path = do
  progS <- readFile path
  let progE = parseProgramSingleError progS
  case progE of
    Left e -> return $ Left e
    Right prog -> do
      return $ createSemanticProgram prog

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
  Either (Either CompilerError RuntimeError) SemExpression
evalStep prog env expS = do
  syntactic <- sinkL $ parseExpression expS
  let (_, semanticE) = runState (createSemanticExpressionS syntactic) env
  semantic <- sinkL semanticE
  sinkR $ normalizePartial prog semantic

replacingStep :: SemProgram -> SemExpression -> SemExpression
replacingStep prog evaluated = fromMaybe evaluated $ findEqDefinition prog evaluated

findEqDefinition :: SemProgram -> SemExpression -> Maybe SemExpression
findEqDefinition prog@(SemProgram _ parts) expr =
  find
    ( \case
        SemDeclaration -> False
        SemDefinition _ e -> alpha prog e expr
    )
    parts
    >>= \case
      SemDeclaration -> Nothing
      SemDefinition p _ -> Just $ SId p

printStep :: Either (Either CompilerError RuntimeError) SemExpression -> IO ()
printStep (Left e) = putStrLn (show e ++ "\n")
printStep (Right a) = putStrLn (show a ++ "\n")
