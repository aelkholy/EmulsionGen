{-# LANGUAGE DeriveGeneric #-}
module Ingredients.SilverNitrate (
  SilverNitrate(..)
) where

import GHC.Generics
import Ingredients.Basics (Chemical(..))

data SilverNitrate = SILVERNITRATE {amount :: Double} deriving (Generic, Show)

-- instance Applicative SilverNitrate where  

instance Chemical SilverNitrate where
  molecularWeight _ = 169.87