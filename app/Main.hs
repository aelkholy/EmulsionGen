{-# LANGUAGE RecordWildCards #-}
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
import Options.Applicative
import Data.Semigroup ((<>))

data Opts = OPTS
  { filePath :: String
  , analyze :: Bool
  , debug :: Bool
  , procedure :: Bool
}

opts :: ParserInfo Opts
opts = info (optionParser <**> helper)
  ( fullDesc
  <> progDesc "EmulsionGen - Analyze silver gelatin emulsions."
  <> header "EmulsionGen by Alexander Elkholy" )

optionParser :: Parser Opts
optionParser = OPTS
    <$> strOption
        ( long "filePath"
        <> short 'f'
        <> metavar "file"
        <> value "data/SteigmannLith.json"
        <> showDefault
        <> help "Input emulsion json file" )
    <*> switch
        ( long "analyze"
        <> short 'A'
        <> help "Run analysis of an emulsion formula" )
    <*> switch
        ( long "debug"
        <> short 'D'
        <> help "Debug and print all state of an emulsion formula" )
    <*> switch
        ( long "procedure"
        <> short 'P'
        <> help "Print recipe / procedure of an emulsion formula" )

main :: IO ()
main = do
  OPTS{..} <- execParser opts
  file <- B.readFile filePath
  when analyze 
    ( case decodeToAnalysis file of
      Left err -> System.IO.putStrLn err
      Right ps -> putStr ( snd $ runWriter ps ))
  when procedure 
    ( case decodeToProcedure file of
      Left err -> System.IO.putStrLn err
      Right ps -> putStr ( snd $ runWriter ps ))
  when debug
    ( case decodeToDebug file of
      Left err -> System.IO.putStrLn err
      Right ps -> putStr ( snd $ runWriter ps ))
