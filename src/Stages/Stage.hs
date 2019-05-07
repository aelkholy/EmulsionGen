module Stages.Stage (
) where

-- Hackage
-- Homies
import Solution                                     as S
import qualified Stages.Digestion                   as D
import qualified Stages.Precipitation               as P
import qualified Ingredients.SilverNitrate          as AG
import Ingredients.ChemicalModifier                 (ChemicalModifier)
import Ingredients.Ingredient                       (Chemical(..))
import Physics                                      (Rate(..), Temperature, Minute)

-- scanl :: (b -> a -> b) -> b -> [a] -> [b]
-- (Solution -> Step -> Solution) -> Solution -> [Step] -> [Solution]
-- The function (Solution -> Step -> Solution) is the mixer function

class Stage a where
    partial initialSolution steps = scanl s.mixer initialSolution steps
    stage :: [P.Solution] -> a

-- instance Stage P.Precipitation where
--     parseSolutions sols = P.SINGLEJET{P.duration=10, P.temperature=Just 10}