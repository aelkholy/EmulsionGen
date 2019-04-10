{-# LANGUAGE DeriveGeneric #-}
module Ingredients.Basics (
  Time, Temperature,
  Unit (..), Quantity (..),
  Rate, Chemical (..), mixQuantities
) where

import GHC.Generics
import Control.Applicative

type Time = Double
type Temperature = Double
data Unit = GRAM | MILLILITER  deriving (Generic, Show)
data Quantity = QUANTITY { amount :: Double, unit :: Unit, conc :: Maybe Double } deriving (Generic, Show) -- conc is %
data Rate = RATE { amountAdded :: Double, overTime :: Time } deriving (Generic, Show) -- percent / over time minutes

class Chemical a where
  molecularWeight :: a -> Maybe Double

mixQuantities :: Quantity -> Quantity -> Quantity
mixQuantities (QUANTITY amtOne GRAM concOne) (QUANTITY amtTwo GRAM concTwo) = QUANTITY { amount = amtOne + amtTwo, unit = GRAM, conc = Nothing}
mixQuantities (QUANTITY amtOne GRAM concOne) (QUANTITY amtTwo MILLILITER concTwo) = ?