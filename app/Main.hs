module Main where

-- MY MODS
import FileLoader (decoder)
-- FROM STACKAGE
import qualified Data.ByteString.Lazy as B
import System.Environment
import System.IO  
import Control.Monad
import Data.Aeson

main :: IO ()
main = do
    args <- getArgs
    case args of
      [] -> error "must supply file"
      [filePath] -> do
            file <- B.readFile filePath
            case decoder file of
                Left err -> System.IO.putStrLn err
                Right ps -> print ps
      _ -> error "too many arguments"
