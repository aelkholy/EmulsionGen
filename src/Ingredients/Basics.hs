{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Ingredients.Basics (
  Time, Temperature,
  Unit (..),
  Rate, Chemical (..)
) where

import GHC.Generics
import Data.Data
import Control.Applicative

type Time = Double
type Temperature = Double
data Unit = GRAM | MILLILITER  deriving (Generic, Show, Typeable, Data)
data Rate = RATE { amountAdded :: Double, overTime :: Time } deriving (Generic, Show) -- percent / over time minutes

class Chemical a where
  molecularWeight :: a -> Double
  grams :: a -> Double
  moles :: a -> Double
  moles x = grams x / molecularWeight x
  molesToGrams :: a -> Double -> Double
  molesToGrams x amt = amt * molecularWeight x
  react :: Chemical b => a -> b -> Double
  react x y = molesToGrams x leftoverMoles
    where leftoverMoles = min (moles x) (moles y)