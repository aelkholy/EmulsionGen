module Main where

-- MY MODS
import Analysis
-- FROM STACKAGE
import qualified Data.ByteString.Lazy as B
import System.Environment
import System.IO  
import Control.Monad
import Data.Aeson
import Control.Monad.Writer

main :: IO ()
main = do
    args <- getArgs
    case args of
      [] -> error "must supply file"
      [filePath] -> do
            file <- B.readFile filePath
            -- case decodeToProcedure file of
            case decodeToAnalysis file of
                Left err -> System.IO.putStrLn err
                Right ps -> putStr ( snd $ runWriter ps )
      _ -> error "too many arguments"
