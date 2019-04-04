module FileLoader ( 
    getJSON
    ) where

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
    -- parseJSON = withObject "solution" $ \o -> do
    --     salts .? "salts"
    --     ag    .? "ag"
    --     acids .? "acids"
    --     other .? "other"
    --     water .? "water"
    --     temp  .? "temp"
    --     return SilverGelatin.Solution{..}

instance ToJSON SilverGelatin.Solution

instance FromJSON SilverGelatin.Rate
instance ToJSON SilverGelatin.Rate

instance FromJSON SilverGelatin.Transition
instance ToJSON SilverGelatin.Transition

instance FromJSON SilverGelatin.Emulsion
instance ToJSON SilverGelatin.Emulsion

--   salts :: [Salt],
--   ag :: Silver,
--   acids :: Acid,
--   other :: [ChemicalModifier],
--   water :: Double,
--   temp :: Temperature