module Stages.Digestion (
  Digestion(..)
  ) where

-- Hackage
-- Homies
import Physics                      (Temperature, Minute)
import Ingredients.ChemicalModifier  (ChemicalModifier)
import Emulsion                     (Solution)

data Digestion = DIGESTION {restingMinutes :: Minute, temperature :: Temperature, additives :: Maybe [ChemicalModifier]}
