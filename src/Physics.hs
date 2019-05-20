{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Physics (
  Second, Minute, Temperature,
  Unit (..), Rate(..),
  prettyUnit, prettyTemperature, prettyRate
) where

import GHC.Generics
import Data.Data

type Second = Double
type Minute = Double
type Temperature = Double
data Unit = GRAM | MILLILITER  deriving (Generic, Show, Typeable, Data, Eq)
data Rate = RATE { amountAdded :: Double, overSeconds :: Second } deriving (Generic, Show) -- percent / over time minutes

prettyUnit :: Unit -> String
prettyUnit GRAM = "grams"
prettyUnit MILLILITER = "milliliters"

prettyTemperature :: Temperature -> String
prettyTemperature t = unwords [show t, "Celsius"]

prettyRate :: Rate -> String
prettyRate (RATE amountAdded 0.0) = unwords ["rate", show $ amountAdded * 100, "%", "immediately"]
prettyRate (RATE amountAdded overSeconds) = unwords ["rate", show $ amountAdded * 100, "%", "over", show overSeconds, "seconds"]