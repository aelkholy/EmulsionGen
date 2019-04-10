module FileLoader ( 
    decoder,
    getJSON
    ) where

import SilverGelatin
-- FROM STACKAGE
import System.Environment
import System.IO  
import Control.Monad
import Data.Aeson

import SilverGelatin
import Ingredients.Basics (Quantity, Time, Temperature, Rate, Unit)
import Ingredients.SilverNitrate (SilverNitrate)
import Ingredients.SilverHalide (SilverHalide)
import Ingredients.Salt (Salt)
import Ingredients.ChemicalModifier (ChemicalModifier)

import qualified Data.ByteString.Lazy as B
import Data.Aeson 
-- exports the following:
-- decode :: FromJSON a => ByteString -> Maybe a
-- encode :: ToJSON a => a -> ByteString
-- eitherDecode :: FromJSON a => ByteString -> Either String a
import Data.Aeson
import Data.Aeson.Types                          ( Parser )

instance FromJSON Unit
instance ToJSON Unit

instance FromJSON Quantity
instance ToJSON Quantity

instance FromJSON SilverHalide
instance ToJSON SilverHalide

instance FromJSON SilverNitrate
instance ToJSON SilverNitrate

instance FromJSON ChemicalModifier
instance ToJSON ChemicalModifier

instance FromJSON Salt
instance ToJSON Salt

instance FromJSON Solution
instance ToJSON Solution

instance FromJSON Rate
instance ToJSON Rate

instance FromJSON Transition
instance ToJSON Transition

instance FromJSON Emulsion
instance ToJSON Emulsion

getJSON :: FilePath -> IO B.ByteString
getJSON path = B.readFile path

decoder :: IO B.ByteString -> IO (Either String SilverGelatin.Emulsion)
decoder arg = (eitherDecode <$> arg)