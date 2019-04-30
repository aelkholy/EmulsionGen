module Stages.Digestion (
  Digestion(..)
  ) where

-- Hackage
-- Homies
import Physics                      (Temperature, Time)
import Ingredients.ChemicalModifier  (ChemicalModifier)
import Emulsion                     (Solution)

data Digestion = DIGESTION {time :: Time, temperature :: Temperature, additives :: Maybe [ChemicalModifier]}
