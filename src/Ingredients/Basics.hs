{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Ingredients.Basics (
  Time, Temperature,
  Unit (..), Quantity (..),
  Rate, Chemical (..), mixQuantities
) where

import GHC.Generics
import Data.Data
import Control.Applicative

type Time = Double
type Temperature = Double
data Unit = GRAM | MILLILITER  deriving (Generic, Show, Typeable, Data)
data Quantity = QUANTITY { amount :: Double, unit :: Unit, conc :: Maybe Double } deriving (Generic, Show, Typeable, Data) -- conc is %
data Rate = RATE { amountAdded :: Double, overTime :: Time } deriving (Generic, Show) -- percent / over time minutes

class Chemical a where
  molecularWeight :: a -> Double

mixQuantities :: Quantity -> Quantity -> Quantity
mixQuantities (QUANTITY amtOne GRAM concOne) (QUANTITY amtTwo GRAM concTwo) = QUANTITY { amount = amtOne + amtTwo, unit = GRAM, conc = Nothing}
mixQuantities 
  (QUANTITY amtOne MILLILITER concOne)
  (QUANTITY amtTwo MILLILITER concTwo) = QUANTITY { amount = amtOne + amtTwo, unit = MILLILITER, conc = out }
                                          where out = concOne >>= (\x -> concTwo >>= (\y -> Just (x + y)))
