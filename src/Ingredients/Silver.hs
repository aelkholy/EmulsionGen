{-# LANGUAGE DeriveGeneric #-}
module Ingredients.Silver (
  Silver
) where

import GHC.Generics

import Ingredients.Basics (Quantity, Chemical(..))

data Silver = SILVER { quantity :: Quantity} deriving (Generic, Show)

instance Chemical Silver where
  molecularWeight (SILVER _)  = Just 169.87