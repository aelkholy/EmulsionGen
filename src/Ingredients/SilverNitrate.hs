{-# LANGUAGE DeriveGeneric #-}
module Ingredients.SilverNitrate (
  SilverNitrate(..), prettyNitrate
) where

import GHC.Generics
import Data.Maybe
-- Home team
import Ingredients.Ingredient (Chemical(..))

newtype SilverNitrate = SILVERNITRATE {gramAmount :: Maybe Double} deriving (Generic, Show)

-- instance Applicative SilverNitrate where  

instance Chemical SilverNitrate where
  molecularWeight _ = 169.87
  grams nitrate = fromMaybe 0.0 (gramAmount nitrate)

prettyNitrate :: SilverNitrate -> String
prettyNitrate (SILVERNITRATE (Just amt)) = unwords ["Silver Nitrate", show amt, "grams"]
prettyNitrate (SILVERNITRATE Nothing) = ""