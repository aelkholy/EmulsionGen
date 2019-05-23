module FileLoader (
    decoder
    , decoderTwo
    ) where

-- Home team
import Emulsion
import Solution
import Physics                              (Rate, Unit)
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

-- Physics

instance FromJSON Rate
instance ToJSON Rate

instance FromJSON Unit
instance ToJSON Unit

-- Ingredients

instance FromJSON SilverHalide
instance ToJSON SilverHalide

instance FromJSON SilverNitrate
instance ToJSON SilverNitrate

instance FromJSON ChemicalModifier
instance ToJSON ChemicalModifier

instance FromJSON Salt
instance ToJSON Salt

-- Emulsion

instance FromJSON Solution
instance ToJSON Solution

instance FromJSON Step
instance ToJSON Step

instance FromJSON Pour
instance ToJSON Pour

instance ToJSON Emulsion
instance FromJSON Emulsion

decoder :: B.ByteString -> Either String (Writer String Solution)
decoder arg = do
        raw <- inputs
        Right $ foldRecipe (followRecipe) (fst raw) (snd raw)
        where inputs = eitherDecode arg :: Either String (Solution, [Step])

-- decoderTwo :: B.ByteString -> Either String (Writer String Emulsion)
-- decoderTwo arg = do
--     raw <- inputs
--     Right $ do
--         tell $ show $ emulsionRunner (fst raw) (snd raw)
--         return $ emulsionRunner (fst raw) (snd raw)
--     where inputs = eitherDecode arg :: Either String (Solution, [Step])

decoderTwo :: B.ByteString -> Either String (Writer String Solution)
decoderTwo arg = do
    raw <- inputs
    let out = emulsionRunner (fst raw) (snd raw)
    Right $ do
          let final = analysis out
          tell $ fst final
          return $ snd final
    where inputs = eitherDecode arg :: Either String (Solution, [Step])