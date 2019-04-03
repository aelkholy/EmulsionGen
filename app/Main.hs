module Main where

import FileLoader
-- FROM STACKAGE
import System.Environment

main :: IO ()
main = do
    args <- getArgs
    case args of
      [] -> error "must supply a file to open"
      [arg] -> do handle <- openFile arg ReadMode
      _ -> error "too many arguments"
    
    