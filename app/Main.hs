module Main (main) where

import Repl
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  if null args
    then do
      print "I need a filename"
      return ()
    else do
      repl $ head args
