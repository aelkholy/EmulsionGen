module Stages.Digestion (
  Digestion(..)
  ) where

-- Hackage
-- Homies
import Emulsion
import Physics                      (Temperature, Minute)
import Ingredients.ChemicalModifier  (ChemicalModifier)

data Digestion = DIGESTION {restingMinutes :: Minute, temperature :: Temperature, additives :: Maybe [ChemicalModifier]}
