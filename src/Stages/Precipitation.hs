module Stages.Precipitation (
  Precipitation(..)
  ) where

-- Hackage
-- Homies
import Physics           (Rate, Temperature, Minute)
import Emulsion          (Solution)

data Precipitation = SINGLEJET { nitrateSolution :: Solution, rate :: Rate, restingMinutes :: Minute, temperature :: Temperature}
  | DOUBLEJET { saltSolution :: Solution, nitrateSolution :: Solution, rate :: Rate, restingMinutes :: Minute, temperature :: Temperature}
  | REVERSE { saltSolution :: Solution, rate :: Rate, restingMinutes :: Minute, temperature :: Temperature}
