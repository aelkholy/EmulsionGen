module Stages.Precipitation (
  Precipitation(..)
  ) where

-- Hackage
-- Homies
import qualified Solution as S
import Physics           (Rate(..), Temperature, Minute)

data Precipitation = SINGLEJET {duration :: Minute, temperature :: Temperature}
  | DOUBLEJET {duration :: Minute, temperature :: Temperature}
  | REVERSE {duration :: Minute, temperature :: Temperature} deriving (Show)

