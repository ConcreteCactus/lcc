module Main (main) where

import System.Environment
import Compiler

main :: IO ()
main = do
  args <- getArgs
  if length args < 2
    then do
      putStrLn "Usage: lcc <sourcecode> <output>"
      return ()
    else do
      src <- readFile $ head args
      let compE = compileFull src
      case compE of
        Left e -> do
          print e
          print src
          return ()
        Right comp -> do
          _ <- writeFile (args !! 1) comp
          return ()

      
