module Main where

-- MY MODS
import qualified Analysis (analyze)
import qualified FileLoader (getJSON, decoder)
-- FROM STACKAGE
import System.Environment
import System.IO  
import Control.Monad

main :: IO ()
main = do
    args <- getArgs
    case args of
      [] -> error "must supply file"
      [filePath] -> do 
            loaded <- FileLoader.decoder (FileLoader.getJSON filePath)
            -- let d = Analysis.analyze loaded
            case loaded of
                Left err -> System.IO.putStrLn err
                Right ps -> print ps
      _ -> error "too many arguments"