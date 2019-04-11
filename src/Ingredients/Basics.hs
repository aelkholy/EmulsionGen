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