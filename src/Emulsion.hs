{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
module Emulsion (
  State
  , moveState
  , stateAnalysis
  -- , analysis
) where

import Control.Monad.Writer
import GHC.Generics
import Data.Aeson
import Data.Maybe                          (fromMaybe, isJust)
import Data.List                           (partition)
--
import Step
import Physics
import Ingredients.Ingredient              (Chemical(..))
import qualified Addition                   as A
import qualified Solution                   as S
import qualified Ingredients.SilverNitrate  as N

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

-- Might move to analysis
data Emulsion a = Nil | Cons a (Emulsion a) deriving (Eq, Show, Functor, Foldable, Traversable)

data State = NOTHING { solution :: S.Solution, additionalSolutions :: [S.Solution], temperature :: Maybe Temperature}
  | NORMAL { solution :: S.Solution, givingSolution :: A.Addition, additionalSolutions :: [S.Solution], duration :: Maybe Minute, temperature :: Maybe Temperature }
  | REVERSE { solution :: S.Solution, givingSolution :: A.Addition, additionalSolutions :: [S.Solution], duration :: Maybe Minute, temperature :: Maybe Temperature }
  | NJET { solution :: S.Solution, givingSolutions :: [A.Addition], additionalSolutions :: [S.Solution], duration :: Maybe Minute, temperature :: Maybe Temperature }
  | GENERICWASH { solution :: S.Solution, additionalSolutions :: [S.Solution] }
  | DIGESTION { solution :: S.Solution, additionalSolutions :: [S.Solution], duration :: Maybe Minute, temperature :: Maybe Temperature }
  deriving (Generic, Show, ToJSON, FromJSON)

-- This can be equivalent to the solution runner with a write monad.
stateAnalysis :: S.Solution -> [Step] -> State
stateAnalysis sol = foldl moveState (NOTHING sol [] Nothing)

-- This will, given a state, give the next state
moveState :: State -> Step -> State
-- Deal with setting the temperature
moveState currentStage (TEMPERATURE newTemp) = case currentStage of
                                                  w@(GENERICWASH s a) -> DIGESTION { solution = S.washSolution $ mixStage w, additionalSolutions=[], duration = Nothing, temperature = Just newTemp}
                                                  other -> other { temperature = Just newTemp }
-- Deal with setting the duration
moveState currentStage (REST minutes) = case currentStage of
                                            NOTHING{} -> currentStage
                                            GENERICWASH{} -> currentStage
                                            other -> other {duration = Just minutes}
-- Deal with entering wash stage
moveState currentStage WASH = case currentStage of
                                  NOTHING {} -> currentStage
                                  w@(GENERICWASH s a) -> GENERICWASH {solution = S.washSolution $ S.washSolution $ mixStage w, additionalSolutions=[]}
                                  d@(DIGESTION s _ _ _) -> GENERICWASH {solution = S.washSolution $ mixStage d, additionalSolutions=[]}
                                  n@(NJET r g _ _ _) -> GENERICWASH {solution = S.washSolution $ mixStage n, additionalSolutions=[]}
                                  other -> GENERICWASH {solution = mixStage other, additionalSolutions=[]}
-- Deal with detecting precipitation
moveState currentStage@NOTHING{} ps@(ADDITION pours) = detectedPrecipitation currentStage pours
moveState currentStage@GENERICWASH{} ps@(ADDITION pours) = detectedPrecipitation currentStage pours
moveState currentStage ps@(ADDITION pours)
  | isJust (duration currentStage) = detectedPrecipitation currentStage pours
  | otherwise = currentStage { solution = foldl S.addSolutions (solution currentStage) (map A.solution (fst partitioned)), additionalSolutions = additionalSolutions currentStage ++ map A.solution (fst partitioned)  }
  where partitioned = partition (\x -> containsNitratePour x || containsSaltPour x) pours 

detectedPrecipitation :: State -> [A.Addition] -> State
detectedPrecipitation base [] = base
detectedPrecipitation base [add]
        | containsNitrate (A.solution add) && containsSalt (mixStage base) = NORMAL { solution = mixStage base, givingSolution = add, additionalSolutions=[], duration = Nothing, temperature = Nothing }
        | containsSalt (A.solution add) && containsNitrate (mixStage base) = REVERSE { solution = mixStage base, givingSolution = add, additionalSolutions=[], duration = Nothing, temperature = Nothing }
        | containsSaltPour add = base { solution = S.addSolutions (solution base) (A.solution add) } -- add salt to base if it contains it
        | containsNitratePour add = base { solution = S.addSolutions (solution base) (A.solution add) } -- add nitrate to base if it contains it
        | otherwise = base { additionalSolutions = additionalSolutions base ++ [A.solution add] } -- otherwise, add to additions
detectedPrecipitation base adds = case partitioned of
                                        ([],a) -> base { additionalSolutions = additionalSolutions base ++ map A.solution a}
                                        ([x],a) | containsSalt baseSolution && containsNitrate (A.solution x) -> NORMAL { solution = mixStage base, givingSolution = x, additionalSolutions=map A.solution a, duration = Nothing, temperature = Nothing }
                                                | containsNitrate baseSolution && containsSalt (A.solution x) -> REVERSE { solution = mixStage base, givingSolution = x, additionalSolutions=map A.solution a, duration = Nothing, temperature = Nothing }
                                                | otherwise -> base { additionalSolutions = additionalSolutions base ++ map A.solution a}
                                        (x,a) -> NJET { solution = mixStage base, givingSolutions = x, additionalSolutions = map A.solution a, duration = Nothing, temperature = Nothing }
                      where baseSolution = solution base
                            partitioned = partition (\x -> containsNitratePour x || containsSaltPour x) adds -- TODO: Deal with n jets properly

mixStage :: State -> S.Solution
mixStage (NOTHING s a _) = foldl S.addSolutions s a
mixStage (GENERICWASH s a) = foldl S.addSolutions s a
mixStage (NJET s g a _ _) = 
        let base = foldl S.addSolutions s a
            next = map A.solution g
        in foldl S.addSolutions base next
mixStage st = 
        let s = solution st
            a = additionalSolutions st
            g = givingSolution st
            base = foldl S.addSolutions s a
            next = A.solution g
        in S.addSolutions base next

containsNitratePour :: A.Addition -> Bool
containsNitratePour p = containsNitrate (A.solution p)

containsSaltPour :: A.Addition -> Bool
containsSaltPour p = containsSalt (A.solution p)

containsNitrate :: S.Solution -> Bool
containsNitrate sol = 
        let nitrate = S.silverNitrate sol
            result = fromMaybe (N.SILVERNITRATE 0.0) nitrate
        in grams result > 0.0

containsSalt :: S.Solution -> Bool
containsSalt sol = 
  let salts = S.salts sol
      result = fromMaybe [] salts
      amts = map grams result
  in sum amts > 0.0