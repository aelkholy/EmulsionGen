module Main where

import FileLoader
-- FROM STACKAGE
import System.Environment
import System.IO

main :: IO ()
main = do
    args <- getArgs
    case args of
      [] -> error "must supply file"
      [arg] -> do 
        handle <- openFile arg ReadMode
        putStrLn "asdf"
      _ -> error "too many arguments"