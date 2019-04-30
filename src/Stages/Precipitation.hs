module Stages.Precipitation (
  Precipitation(..)
  ) where

-- Hackage
-- Homies
import Physics           (Rate, Temperature, Time)
import Emulsion          (Solution)

data Precipitation = SINGLEJET { nitrateSolution :: Solution, rate :: Rate, time :: Time, temperature :: Temperature}
  | DOUBLEJET { saltSolution :: Solution, nitrateSolution :: Solution, rate :: Rate, time :: Time, temperature :: Temperature}
  | REVERSE { saltSolution :: Solution, rate :: Rate, time :: Time, temperature :: Temperature} 
