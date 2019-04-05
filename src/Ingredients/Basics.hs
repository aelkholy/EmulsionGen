{-# LANGUAGE DeriveGeneric #-}
module Ingredients.Basics (
  Time, Temperature,
  Unit, Quantity,
  Rate, Chemical (..)
) where

import GHC.Generics

type Time = Double
type Temperature = Double
data Unit = GRAM | MILLILITER  deriving (Generic, Show)
data Quantity = QUANTITY {amount :: Double, unit :: Unit, conc :: Maybe Double } deriving (Generic, Show) -- conc is %
data Rate = RATE { amountAdded :: Double, overTime :: Time } deriving (Generic, Show) -- percent / over time minutes

class Chemical a where
  molecularWeight :: a -> Maybe Double