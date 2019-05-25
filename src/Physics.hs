{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module Physics (
  Second, Minute, Temperature
  , Unit (..)
  , Rate(..)
  , prettyUnit
  , prettyTemperature
  , prettyRate
  , prettyMinute
  , prettySecond
) where

import GHC.Generics
import Data.Aeson

type Second = Integer
type Minute = Integer
type Temperature = Double
data Unit = GRAM | MILLILITER  deriving (Generic, Show, Eq, ToJSON, FromJSON)
data Rate = RATE { amountAdded :: Double, overSeconds :: Second } deriving (Generic, Show, ToJSON, FromJSON) -- percent / over time minutes

prettyUnit :: Unit -> String
prettyUnit GRAM = "grams"
prettyUnit MILLILITER = "milliliters"

prettyMinute :: Minute -> String
prettyMinute m 
  | m >= 60 = unwords [show (quot m 60), "hour(s) and", show (mod m 60), "minutes"]
  | otherwise = unwords [show m, "minutes"]

prettySecond :: Second -> String
prettySecond m 
  | m >= 60 = unwords [show (quot m 60), "minute(s) and", show (mod m 60), "seconds"]
  | otherwise = unwords [show m, "seconds"]

prettyTemperature :: Temperature -> String
prettyTemperature t = unwords [show t, "Celsius"]

prettyRate :: Rate -> String
prettyRate (RATE amountAdded 0) = unwords ["rate", show $ amountAdded * 100, "%", "immediately"]
prettyRate (RATE amountAdded overSeconds) = unwords ["rate", show $ amountAdded * 100, "%", "over", show overSeconds, "seconds"]