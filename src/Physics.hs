{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Physics (
  Time, Temperature,
  Unit (..), Rate(..),
  prettyUnit, prettyTemperature
) where

import GHC.Generics
import Data.Data

type Time = Double
type Temperature = Maybe Double
data Unit = GRAM | MILLILITER  deriving (Generic, Show, Typeable, Data)
data Rate = RATE { amountAdded :: Double, overTime :: Time } deriving (Generic, Show) -- percent / over time minutes

prettyUnit :: Unit -> String
prettyUnit GRAM = "grams"
prettyUnit MILLILITER = "milliliters"

prettyTemperature :: Temperature -> String
prettyTemperature (Just t) = unwords [show t, "Celsius"]
prettyTemperature Nothing = ""