{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module Physics (
  Second, Minute, Temperature
  , Unit (..)
  , Rate(..)
  , PowerHydrogen
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
data Rate = RATE { amountAdded :: Double, overSeconds :: Second } deriving (Generic, Show, Eq, ToJSON, FromJSON) -- percent / over time minutes

newtype PowerHydrogen = PH Double deriving (Generic, ToJSON, FromJSON)

instance Show PowerHydrogen where 
  show (PH x)
    | x >= 12.5 = unwords [show x, "(very alkaline)"]
    | x > 7 = unwords [show x, "(slightly alkaline)"]
    | x == 7 = unwords [show x, "(neutral)"]
    | x >= 3.5 = unwords [show x, "(slightly acidic)"]
    | x >= 0 = unwords [show x, "(very acidic)"]
instance Ord PowerHydrogen where compare (PH x) (PH y) = compare x y
instance Eq PowerHydrogen where (==) (PH x) (PH y) = x == y

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
prettyRate (RATE amountAdded 0) = unwords [show $ amountAdded * 100, "%", "immediately"]
prettyRate (RATE amountAdded overSeconds) = unwords [show $ amountAdded * 100, "%", "over", show overSeconds, "seconds"]