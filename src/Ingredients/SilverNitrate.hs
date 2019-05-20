{-# LANGUAGE DeriveGeneric #-}
module Ingredients.SilverNitrate (
  SilverNitrate(..), mergeNitrate,
  prettyNitrate
) where

import GHC.Generics
import Data.Maybe
-- Home team
import Ingredients.Ingredient (Chemical(..))

newtype SilverNitrate = SILVERNITRATE {gramAmounts :: Double} deriving (Generic, Show)

-- instance Applicative SilverNitrate where  

instance Chemical SilverNitrate where
  molecularWeight _ = 169.87
  grams = gramAmounts

prettyNitrate :: SilverNitrate -> String
prettyNitrate (SILVERNITRATE amt) = unwords ["Silver Nitrate", show amt, "grams"]

mergeNitrate :: Maybe SilverNitrate -> Maybe SilverNitrate -> SilverNitrate
mergeNitrate Nothing Nothing = SILVERNITRATE{gramAmounts=0.0}
mergeNitrate (Just x) Nothing = x
mergeNitrate Nothing (Just x) = x
mergeNitrate (Just (SILVERNITRATE one)) (Just(SILVERNITRATE two)) = SILVERNITRATE{gramAmounts=one + two}