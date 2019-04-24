{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Ingredients.Basics (
  Time, Temperature,
  Unit (..), prettyUnit,
  Rate, Chemical (..)
) where

import GHC.Generics
import Data.Data
import Control.Applicative

type Time = Double
type Temperature = Double
data Unit = GRAM | MILLILITER  deriving (Generic, Show, Typeable, Data)
data Rate = RATE { amountAdded :: Double, overTime :: Time } deriving (Generic, Show) -- percent / over time minutes

prettyUnit :: Unit -> String
prettyUnit GRAM = "grams"
prettyUnit MILLILITER = "milliliters"


class Chemical a where
  molecularWeight :: a -> Double
  grams :: a -> Double
  moles :: a -> Double
  moles x = grams x / molecularWeight x
  molesToGrams :: a -> Double -> Double
  molesToGrams x amt = amt * molecularWeight x
  reactForAmountB :: Chemical b => a -> b -> Double
  reactForAmountB x y = leftoverMoles
    where leftoverMoles = min (moles x) (moles y)
  reactForLeftoverA :: Chemical b => a -> b -> Double
  reactForLeftoverA x y = molesToGrams x finalMoles
    where leftoverMoles = min (moles x) (moles y)
          finalMoles = moles x - leftoverMoles