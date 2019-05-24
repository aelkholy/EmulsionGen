{-# LANGUAGE DeriveGeneric #-}
module Emulsion (
  Emulsion
  , stage
  , emulsionRunner
  , analysis
) where

import Control.Monad.Writer
import GHC.Generics
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

data Emulsion = NOTHING { solution :: S.Solution, additionalSolutions :: [S.Solution], temperature :: Maybe Temperature}
  | NORMAL { solution :: S.Solution, givingSolution :: A.Addition, additionalSolutions :: [S.Solution], duration :: Maybe Minute, temperature :: Maybe Temperature, emulsion :: Emulsion }
  | REVERSE { solution :: S.Solution, givingSolution :: A.Addition, additionalSolutions :: [S.Solution], duration :: Maybe Minute, temperature :: Maybe Temperature, emulsion :: Emulsion }
  | NJET { solution :: S.Solution, givingSolutions :: [A.Addition], additionalSolutions :: [S.Solution], duration :: Maybe Minute, temperature :: Maybe Temperature, emulsion :: Emulsion }
  | GENERICWASH { solution :: S.Solution, additionalSolutions :: [S.Solution], emulsion :: Emulsion }
  | DIGESTION { solution :: S.Solution, additionalSolutions :: [S.Solution], duration :: Maybe Minute, temperature :: Maybe Temperature, emulsion :: Emulsion }
  deriving (Generic, Show)

-- Don't feel like bringing in the writer monad
analysis :: Emulsion -> (String, S.Solution)
analysis e@(NOTHING s a t) = let
        text = unwords [
            "START SOLUTION:\n",
            S.prettySolution s,
            "_w/ additions:\n",
            unwords $ map S.prettySolution a
          ]
        in (text, mixStage e)
analysis e@(NORMAL s g a d t ep) = let
        thisText = unwords [
            "NORMAL PRECIPITATION:\n",
            "Add solution:\n",
            A.prettyAddition g,
            "Into solution:\n",
            S.prettySolution s,
            "Precipitation for:\n",
            maybe "UNKNOWN DURATION" prettyMinute d,
            "At temperature:\n",
            maybe "UNKNOWN TEMPERATURE" prettyTemperature t
          ]
        previous = analysis ep
        previousText = fst previous
        previousSoln = snd previous
        in (unwords [thisText, previousText], S.addSolutions previousSoln $ mixStage e)
analysis e@(REVERSE s g a d t ep) = let
          thisText = unwords [
              "REVERSE PRECIPITATION:\n",
              "Add solution:\n",
              A.prettyAddition g,
              "Into solution:\n",
              S.prettySolution s,
              "Precipitation for:\n",
              maybe "UNKNOWN DURATION" prettyMinute d,
              "At temperature:\n",
              maybe "UNKNOWN TEMPERATURE" prettyTemperature t
            ]
          previous = analysis ep
          previousText = fst previous
          previousSoln = snd previous
          in (unwords [thisText, previousText], S.addSolutions previousSoln $ mixStage e)
analysis e@(NJET s gs a d t ep) = let
            thisText = unwords $ [
                "N-JET PRECIPITATION:\n",
                "Jet the following solutions:\n"]
                ++ map A.prettyAddition gs ++ [
                "Into solution:\n",
                S.prettySolution s,
                "Precipitation for:\n",
                maybe "UNKNOWN DURATION" prettyMinute d,
                "At temperature:\n",
                maybe "UNKNOWN TEMPERATURE" prettyTemperature t
              ]
            previous = analysis ep
            previousText = fst previous
            previousSoln = snd previous
            in (unwords [thisText, previousText], S.addSolutions previousSoln $ mixStage e)
analysis e@(GENERICWASH s a ep) = let
              thisText = unwords $ [
                  "WASH EMULSION:\n",
                  "Solution looks like this:\n",
                  S.prettySolution s,
                  "Additionally add:\n"]
                  ++ map S.prettySolution a
              previous = analysis ep
              previousText = fst previous
              previousSoln = snd previous
              in (unwords [thisText, previousText], S.addSolutions previousSoln $ mixStage e)
analysis e@(DIGESTION s a d t ep) = let
                thisText = unwords $ [
                    "DIGESTION:\n",
                    "Add the following:\n"]
                    ++ map S.prettySolution a ++ [
                    "Digest at:\n",
                    maybe "UNKNOWN DURATION" prettyMinute d,
                    "At temperature:\n",
                    maybe "UNKNOWN TEMPERATURE" prettyTemperature t
                  ]
                previous = analysis ep
                previousText = fst previous
                previousSoln = snd previous
                in (unwords [thisText, previousText], S.addSolutions previousSoln $ mixStage e)

emulsionRunner :: S.Solution -> [Step] -> Emulsion
emulsionRunner sol = foldl stage (NOTHING sol [] Nothing)

stage :: Emulsion -> Step -> Emulsion
-- Deal with setting the temperature
stage currentStage (TEMPERATURE newTemp) = case currentStage of
                                                  w@(GENERICWASH s a e) -> DIGESTION { solution = S.washSolution $ mixStage w, additionalSolutions=[], duration = Nothing, temperature = Just newTemp, emulsion = currentStage}
                                                  other -> other { temperature = Just newTemp }
-- Deal with setting the duration
stage currentStage (REST minutes) = case currentStage of
                                            NOTHING{} -> currentStage
                                            GENERICWASH{} -> currentStage
                                            other -> other {duration = Just minutes}
-- Deal with entering wash stage
stage currentStage WASH = case currentStage of
                                  NOTHING {} -> currentStage
                                  w@(GENERICWASH s a e) -> GENERICWASH {solution = S.washSolution $ S.washSolution $ mixStage w, additionalSolutions=[], emulsion = currentStage}
                                  d@(DIGESTION s _ _ _ e) -> GENERICWASH {solution = S.washSolution $ mixStage d, additionalSolutions=[], emulsion = currentStage}
                                  n@(NJET r g _ _ _ e) -> GENERICWASH {solution = S.washSolution $ mixStage n, additionalSolutions=[], emulsion = currentStage}
                                  other -> GENERICWASH {solution = mixStage other, additionalSolutions=[], emulsion = currentStage}
-- Deal with detecting precipitation
stage currentStage@NOTHING{} ps@(ADDITION pours) = detectedPrecipitation currentStage pours
stage currentStage@GENERICWASH{} ps@(ADDITION pours) = detectedPrecipitation currentStage pours
stage currentStage ps@(ADDITION pours)
  | isJust (duration currentStage) = detectedPrecipitation currentStage pours
  | otherwise = currentStage { solution = foldl S.addSolutions (solution currentStage) (map A.solution (fst partitioned)), additionalSolutions = additionalSolutions currentStage ++ map A.solution (fst partitioned)  }
  where partitioned = partition (\x -> containsNitratePour x || containsSaltPour x) pours 

detectedPrecipitation :: Emulsion -> [A.Addition] -> Emulsion
detectedPrecipitation base [] = base
detectedPrecipitation base [add]
        | containsNitrate (A.solution add) && containsSalt (mixStage base) = NORMAL { solution = mixStage base, givingSolution = add, additionalSolutions=[], duration = Nothing, temperature = Nothing, emulsion = base }
        | containsSalt (A.solution add) && containsNitrate (mixStage base) = REVERSE { solution = mixStage base, givingSolution = add, additionalSolutions=[], duration = Nothing, temperature = Nothing, emulsion = base }
        | containsSaltPour add = base { solution = S.addSolutions (solution base) (A.solution add) } -- add salt to base if it contains it
        | containsNitratePour add = base { solution = S.addSolutions (solution base) (A.solution add) } -- add nitrate to base if it contains it
        | otherwise = base { additionalSolutions = additionalSolutions base ++ [A.solution add] } -- otherwise, add to additions
detectedPrecipitation base adds = case partitioned of
                                        ([],a) -> base { additionalSolutions = additionalSolutions base ++ map A.solution a}
                                        ([x],a) | containsSalt baseSolution && containsNitrate (A.solution x) -> NORMAL { solution = mixStage base, givingSolution = x, additionalSolutions=map A.solution a, duration = Nothing, temperature = Nothing, emulsion = base }
                                                | containsNitrate baseSolution && containsSalt (A.solution x) -> REVERSE { solution = mixStage base, givingSolution = x, additionalSolutions=map A.solution a, duration = Nothing, temperature = Nothing, emulsion = base }
                                                | otherwise -> base { additionalSolutions = additionalSolutions base ++ map A.solution a}
                                        (x,a) -> NJET { solution = mixStage base, givingSolutions = x, additionalSolutions = map A.solution a, duration = Nothing, temperature = Nothing, emulsion = base }
                      where baseSolution = solution base
                            partitioned = partition (\x -> containsNitratePour x || containsSaltPour x) adds -- TODO: Deal with n jets properly

mixStage :: Emulsion -> S.Solution
mixStage (NOTHING s a _) = foldl S.addSolutions s a
mixStage (GENERICWASH s a _) = foldl S.addSolutions s a
mixStage (NJET s g a _ _ _) = 
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