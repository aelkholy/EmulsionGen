{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module Step (
    Step(..)
    , moveStep
  ) where

-- Hackage
import GHC.Generics
import Data.Aeson
-- Homies
import Physics
import Ingredients.Ingredient        (Chemical(..))
import Ingredients.ChemicalModifier   (ChemicalModifier(..))
import qualified Solution             as S
import qualified Addition             as A

-- Steps you can take in a recipe

data Step = TEMPERATURE {setCelsius :: Temperature}
 | ADDITION {additions :: [A.Addition]}
 | PH {addition :: ChemicalModifier, pH :: PowerHydrogen}
 | REST {minutes :: Minute}
 | WASH deriving (Generic, Show, ToJSON, FromJSON)

moveStep :: S.Solution -> Step -> S.Solution
moveStep soln (TEMPERATURE t) = soln
moveStep soln (REST t) = soln
moveStep soln (PH c ph) = soln {S.otherChemicals = fmap (c:) (S.otherChemicals soln)}
moveStep soln WASH = S.washSolution soln
moveStep soln (ADDITION sols) = foldl S.addSolutions x xs
    where x = A.solution $ head sols
          xs = map A.solution $ tail sols