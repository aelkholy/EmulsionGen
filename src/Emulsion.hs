{-# LANGUAGE DeriveGeneric #-}
module Emulsion (
  Emulsion
  , stage
  , emulsionRunner
  , analysis
) where

import Control.Monad.Writer
import GHC.Generics
import Data.Maybe                          (fromMaybe)
import Data.List                           (partition)
--
import Physics
import Ingredients.Ingredient              (Chemical(..))
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

data Emulsion = NOTHING { solution :: S.Solution, additions :: [S.Solution], temperature :: Maybe Temperature}
  | NORMAL { solution :: S.Solution, givingSolution :: S.Pour, additions :: [S.Solution], duration :: Maybe Minute, temperature :: Maybe Temperature, emulsion :: Emulsion }
  | REVERSE { solution :: S.Solution, givingSolution :: S.Pour, additions :: [S.Solution], duration :: Maybe Minute, temperature :: Maybe Temperature, emulsion :: Emulsion }
  | NJET { solution :: S.Solution, givingSolutions :: [S.Pour], additions :: [S.Solution], duration :: Maybe Minute, temperature :: Maybe Temperature, emulsion :: Emulsion }
  | WASH { solution :: S.Solution, additions :: [S.Solution], emulsion :: Emulsion }
  | DIGESTION { solution :: S.Solution, additions :: [S.Solution], duration :: Maybe Minute, temperature :: Maybe Temperature, emulsion :: Emulsion }
  deriving (Generic, Show)

analysis :: Emulsion -> (String, S.Solution)
analysis e@(NOTHING s a t) = let
                              text = unwords [
                                  "Starting solution:\n",
                                  S.prettySolution s,
                                  "w/ additions:\n",
                                  unwords (map S.prettySolution a)
                                ]
                             in (text, mixStage e)
analysis e = let
                                out = analysis (emulsion e)
                                text = unwords ["asdf", fst out]
                                soln = S.addSolutions (snd out) (mixStage e)
                             in (text, mixStage e)

emulsionRunner :: S.Solution -> [S.Step] -> Emulsion
emulsionRunner sol = foldl stage (NOTHING sol [] Nothing)

stage :: Emulsion -> S.Step -> Emulsion
-- Deal with setting the temperature
stage currentStage (S.TEMPERATURE newTemp) = case currentStage of
                                                  w@(WASH s a e) -> DIGESTION { solution = mixStage w, additions=[], duration = Nothing, temperature = Just newTemp, emulsion = currentStage}
                                                  other -> other { temperature = Just newTemp }
-- Deal with setting the duration
stage currentStage (S.REST minutes) = case currentStage of
                                            NOTHING{} -> currentStage
                                            WASH{} -> currentStage
                                            other -> other {duration = Just minutes}
-- Deal with entering wash stage
stage currentStage S.WASH = case currentStage of
                                  NOTHING {} -> currentStage
                                  w@(WASH s a e) -> WASH {solution = S.washSolution $ mixStage w, additions=[], emulsion = currentStage}
                                  d@(DIGESTION s _ _ _ e) -> WASH {solution = S.washSolution $ mixStage d, additions=[], emulsion = currentStage}
                                  n@(NJET r g _ _ _ e) -> WASH {solution = S.washSolution $ mixStage n, additions=[], emulsion = currentStage}
                                  other -> WASH {solution = S.washSolution $ mixStage other, additions=[], emulsion = currentStage}
-- Deal with detecting precipitation
stage currentStage ps@(S.ADDITION pours) = detectedPrecipitation currentStage pours

detectedPrecipitation :: Emulsion -> [S.Pour] -> Emulsion
detectedPrecipitation base [] = base
detectedPrecipitation base [add] 
        | containsNitrate (S.solution add) && containsSalt (solution base) = NORMAL { solution = mixStage base, givingSolution = add, additions=[], duration = Nothing, temperature = Nothing, emulsion = base }
        | containsSalt (S.solution add) && containsNitrate (solution base) = REVERSE { solution = mixStage base, givingSolution = add, additions=[], duration = Nothing, temperature = Nothing, emulsion = base }
        | containsSaltPour add = base { solution = S.addSolutions (solution base) (S.solution add) }
        | containsNitratePour add = base { solution = S.addSolutions (solution base) (S.solution add) }
        | otherwise = base { additions = additions base ++ [S.solution add] }
detectedPrecipitation base adds = case partitioned of
                                        ([],a) -> base { additions = additions base ++ map S.solution a}
                                        ([x],a) | containsSalt baseSolution && containsNitrate (S.solution x) -> NORMAL { solution = mixStage base, givingSolution = x, additions=map S.solution a, duration = Nothing, temperature = Nothing, emulsion = base }
                                                | containsNitrate baseSolution && containsSalt (S.solution x) -> REVERSE { solution = mixStage base, givingSolution = x, additions=map S.solution a, duration = Nothing, temperature = Nothing, emulsion = base }
                                                | otherwise -> base { additions = additions base ++ map S.solution a}
                                        (x,a) -> NJET { solution = mixStage base, givingSolutions = x, additions = map S.solution a, duration = Nothing, temperature = Nothing, emulsion = base }
                      where baseSolution = solution base
                            partitioned = partition (\x -> containsNitratePour x || containsSaltPour x) adds -- TODO: Deal with n jet or case

mixStage :: Emulsion -> S.Solution
mixStage (NOTHING s a _) = foldl S.addSolutions s a
mixStage (WASH s a _) = foldl S.addSolutions s a
mixStage (NJET s g a _ _ _) = 
        let base = foldl S.addSolutions s a
            next = map S.solution g
        in foldl S.addSolutions base next
mixStage st = 
        let s = solution st
            a = additions st
            g = givingSolution st
            base = foldl S.addSolutions s a
            next = S.solution g
        in S.addSolutions base next

containsNitratePour :: S.Pour -> Bool
containsNitratePour p = containsNitrate (S.solution p)

containsSaltPour :: S.Pour -> Bool
containsSaltPour p = containsSalt (S.solution p)

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