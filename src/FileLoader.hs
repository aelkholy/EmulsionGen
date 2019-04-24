module FileLoader (
    decoder
    ) where

-- Home team
import SilverGelatin
import Ingredients.Basics                   (Time, Temperature, Rate, Unit)
import Ingredients.SilverNitrate            (SilverNitrate)
import Ingredients.SilverHalide             (SilverHalide)
import Ingredients.Salt                     (Salt)
import Ingredients.ChemicalModifier          (ChemicalModifier)
-- FROM STACKAGE
import Data.Aeson                           ( FromJSON, ToJSON, eitherDecode )
import Data.Aeson.Types                     ( Parser )
import Control.Monad
import Control.Monad.Writer
import System.IO  
import System.Environment
import qualified Data.ByteString.Lazy as B

-- decode :: FromJSON a => ByteString -> Maybe a
-- encode :: ToJSON a => a -> ByteString
-- eitherDecode :: FromJSON a => ByteString -> Either String a

instance FromJSON Unit
instance ToJSON Unit

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

instance FromJSON Step
instance ToJSON Step

decoder :: B.ByteString -> Either String (Writer String Solution)
decoder arg = do
        raw <- inputs
        Right $ foldEmulsion (followRecipe) (fst raw) (snd raw)
        where inputs = eitherDecode arg :: Either String (Solution, [Step])