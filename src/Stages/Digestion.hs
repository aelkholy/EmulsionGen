module Stages.Digestion (
  Digestion(..)
  ) where

-- Hackage
-- Homies
import Solution
import Physics                      (Temperature, Minute)
import Ingredients.ChemicalModifier  (ChemicalModifier)

data Digestion = DIGESTION {duration :: Minute, temperature :: Temperature, finals :: [ChemicalModifier]}
