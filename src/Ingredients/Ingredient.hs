module Ingredients.Ingredient (
 Chemical (..)
) where

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
  compareQuantity :: (Double -> Double -> Bool) -> a -> Double -> Bool
  compareQuantity f a = f (grams a)