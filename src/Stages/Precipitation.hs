module Stages.Precipitation (
  Precipitation(..)
  ) where

-- Hackage
-- Homies
import Emulsion
import Physics           (Rate, Temperature, Minute)

data Precipitation = SINGLEJET { saltSolution :: Solution, nitrateSolution :: Solution, rate :: Rate, restingMinutes :: Maybe Minute, temperature :: Maybe Temperature}
  | DOUBLEJET { saltSolution :: Solution, nitrateSolution :: Solution, saltRate :: Rate, nitrateRate :: Rate, restingMinutes :: Maybe Minute, temperature :: Maybe Temperature}
  | REVERSE { saltSolution :: Solution, nitrateSolution :: Solution, rate :: Rate, restingMinutes :: Maybe Minute, temperature :: Maybe Temperature}
