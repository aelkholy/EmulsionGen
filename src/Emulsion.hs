-- {-# LANGUAGE LambdaCase #-}
module Emulsion (
  Emulsion
  , stage
) where

import Data.List                            (unfoldr, tails)
import Data.Maybe                           (fromMaybe)
import Control.Monad.State
--
import Physics
import qualified Solution                   as S
import qualified Ingredients.SilverNitrate  as N
import Ingredients.Ingredient               (Chemical(..))

-- Questions one. What stages does this emulsion go through?
-- How many precipitations does it have,
-- is there a wash,
-- is there more than one precipitation,
-- is there digestion,
-- what kinds of precipitations?

-- Questions two. What properties does it have w.r.t. stage?
-- What is the gelatin content during precipitation?
-- Is there additional make up gelatin?
-- What does the final species look like?
-- What is the pH during precipitation?

data Stage = NOTHING { solution :: S.Solution, temperature :: Maybe Temperature, emulsion :: Emulsion }
  | NORMAL { solution :: S.Solution, givingSolution :: S.Pour, duration :: Maybe Minute, temperature :: Maybe Temperature, emulsion :: Emulsion }
  | REVERSE { solution :: S.Solution, givingSolution :: S.Pour, duration :: Maybe Minute, temperature :: Maybe Temperature, emulsion :: Emulsion }
  | NJET { solution :: S.Solution, givingSolutions :: [S.Pour], duration :: Maybe Minute, temperature :: Maybe Temperature, emulsion :: Emulsion }
  | WASH { solution :: S.Solution, emulsion :: Emulsion }
  | DIGESTION { solution :: S.Solution, duration :: Maybe Minute, temperature :: Maybe Temperature, emulsion :: Emulsion }
  deriving Show

stage :: Stage -> S.Step -> Stage
-- Deal with setting the temperature
stage currentStage (S.TEMPERATURE newTemp) = case currentStage of
                                                  (WASH s e) -> DIGESTION { solution = s, duration = Nothing, temperature = Just newTemp, emulsion = currentStage}
                                                  other -> other { temperature = Just newTemp }
-- Deal with setting the duration
stage currentStage (S.REST minutes) = case currentStage of
                                            (NOTHING _ _ _) -> currentStage
                                            (WASH _ _) -> currentStage
                                            other -> other {duration = Just minutes}
-- Deal with entering wash stage
stage currentStage S.WASH = case currentStage of
                                  (NOTHING _ _ _) -> currentStage
                                  (WASH s e) -> WASH {solution = s, emulsion = currentStage}
                                  (DIGESTION s _ _ e) -> WASH {solution = s, emulsion = currentStage}
                                  (NJET r g _ _ e) -> WASH {solution = foldl S.addSolutions r (map S.solution g), emulsion = currentStage}
                                  other -> WASH {solution = S.addSolutions (solution other) (S.solution $ givingSolution other), emulsion = currentStage}
-- Deal with detecting precipitation
stage currentStage ps@(S.ADDITION pours) = detectedPrecipitation currentStage ps


detectedPrecipitation :: Stage -> [S.Pour] -> Stage
detectedPrecipitation base [] = base
detectedPrecipitation base [add] 
        | containsNitrate (S.solution add) && containsSalt (solution base) = NORMAL { solution = mixStage base, givingSolution = add, duration = Nothing, temperature = Nothing, emulsion = base }
        | containsSalt (S.solution add) && containsNitrate (solution base) = REVERSE { solution = mixStage base, givingSolution = add, duration = Nothing, temperature = Nothing, emulsion = base }
        | otherwise = base { solution = S.addSolutions (solution base) (S.solution add)}
detectedPrecipitation base adds = case mapToNitrateSalts adds of
                                        0 -> base {solution = foldl S.addSolutions (solution base) (map S.solution add)}
                                        1 | (containsNitrate base && or map containsSalt (map S.solution adds)) || (containsSalt base && or map containsNitrate (map S.solution adds)) -> base
                                        _ -> NJET { }
        where 
          mapToNitrateSalts :: [S.Pour] -> Int

mixStage :: Stage -> S.Solution