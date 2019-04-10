{-# LANGUAGE DeriveGeneric #-}
module Ingredients.Silver (
  Silver, defaultSilver
) where

import GHC.Generics

import Ingredients.Basics (Quantity(..), Chemical(..), Unit(..))

data Silver = SILVER { quantity :: Quantity } deriving (Generic, Show)

defaultSilver = SILVER { quantity = QUANTITY { amount = 0.0, unit = GRAM, conc = Nothing}}

instance Chemical Silver where
  molecularWeight (SILVER _)  = Just 169.87