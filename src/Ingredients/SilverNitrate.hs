{-# LANGUAGE DeriveGeneric #-}
module Ingredients.SilverNitrate (
  SilverNitrate(..), prettyNitrate
) where

import GHC.Generics
import Ingredients.Basics (Chemical(..))

newtype SilverNitrate = SILVERNITRATE {amount :: Double} deriving (Generic, Show)

-- instance Applicative SilverNitrate where  

instance Chemical SilverNitrate where
  molecularWeight _ = 169.87
  grams = amount

prettyNitrate :: SilverNitrate -> String
prettyNitrate (SILVERNITRATE amt) = unwords [show amt, "grams"]