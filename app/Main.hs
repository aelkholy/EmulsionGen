module Main where

import FileLoader
-- FROM STACKAGE
import System.Environment
import System.IO

main :: IO ()
main = do
    args <- getArgs
    putStrLn "Ack"
    -- case args of
    --   [] -> error "must supply file"
    --   [arg] -> do 
    --     handle <- openFile arg ReadMode
    --     FileLoader.parse handle
    --   _ -> error "too many arguments"