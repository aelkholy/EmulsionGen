{-# LANGUAGE DeriveFoldable #-}
module Emulsion (
  Emulsion
) where

--
import Physics
import qualified Solution             as S

-- What questions do I want to ask?
-- What stages does an emulsion go through
-- Because later I want to infer properties and impute based off of these
-- {duration :: Minute, temperature :: Temperature, finals :: [ChemicalModifier]}

data Emulsion a = PRECIPITATION { duration :: Minute, temperature :: Temperature, solution :: a, emulsion :: Emulsion a}
  | DIGESTION { duration :: Minute, temperature :: Temperature, solution :: a, emulsion :: Emulsion a}
  | FINISHED a 
  deriving (Foldable, Show)

-- -- Grow tree
-- chemicalLab :: S.Solution -> [S.Step] -> Emulsion S.Solution
-- chemicalLab solution (s:teps) = 

-- -- Reduce tree