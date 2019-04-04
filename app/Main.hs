module Main where

-- import FileLoader
import SilverGelatin
-- FROM STACKAGE
import System.Environment
import System.IO  
import Control.Monad
import Data.Aeson 
-- import Data.ByteString.Lazy.Char8

import SilverGelatin
import qualified Data.ByteString.Lazy as B
import Data.Aeson 
-- exports the following:
-- decode :: FromJSON a => ByteString -> Maybe a
-- encode :: ToJSON a => a -> ByteString
-- eitherDecode :: FromJSON a => ByteString -> Either String a
import Data.Aeson
import Data.Aeson.Types                          ( Parser )


getJSON :: FilePath -> IO B.ByteString
getJSON path = B.readFile path

instance FromJSON SilverGelatin.Unit
instance ToJSON SilverGelatin.Unit

instance FromJSON SilverGelatin.Quantity
instance ToJSON SilverGelatin.Quantity

instance FromJSON SilverGelatin.Silver
instance ToJSON SilverGelatin.Silver

instance FromJSON SilverGelatin.ChemicalModifier
instance ToJSON SilverGelatin.ChemicalModifier

instance FromJSON SilverGelatin.Salt
instance ToJSON SilverGelatin.Salt

instance FromJSON SilverGelatin.Solution
instance ToJSON SilverGelatin.Solution

instance FromJSON SilverGelatin.Rate
instance ToJSON SilverGelatin.Rate

instance FromJSON SilverGelatin.Transition
instance ToJSON SilverGelatin.Transition

instance FromJSON SilverGelatin.Emulsion
instance ToJSON SilverGelatin.Emulsion

main :: IO ()
main = do
    args <- getArgs
    -- putStrLn "Ack"
    case args of
      [] -> error "must supply file"
      [arg] -> do 
        -- jsonFile <- FileLoader.getJSON arg
        -- System.IO.putStrLn (Data.ByteString.Lazy.Char8.unpack jsonFile)
            d <- (eitherDecode <$> getJSON arg) :: IO (Either String SilverGelatin.Emulsion)
            case d of
                Left err -> System.IO.putStrLn err
                Right ps -> print ps
      _ -> error "too many arguments"
        -- d <- (eitherDecode <$> getJSON) :: IO (Either String [Person])
        --  case d of
        --     Left err -> putStrLn err
        --     Right ps -> print ps