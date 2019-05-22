-- {-# LANGUAGE LambdaCase #-}
module Emulsion (
  Emulsion
  , stage
) where

import Data.List                         (unfoldr, tails)
import Data.Maybe                        (fromMaybe)
import Control.Monad.State
--
import Physics
import qualified Solution                  as S
import qualified Ingredients.SilverNitrate as N
import Ingredients.Ingredient             (Chemical(..))

data Emulsion = START
  | NORMAL { receivingSolution :: S.Solution, givingSolution :: S.Pour, duration :: Maybe Minute, temperature :: Maybe Temperature, emulsion :: Emulsion }
  | REVERSE { receivingSolution :: S.Solution, givingSolution :: S.Pour, duration :: Maybe Minute, temperature :: Maybe Temperature, emulsion :: Emulsion }
  | DOUBLEJET { receivingSolution :: S.Solution, givingSolutions :: [S.Pour], duration :: Maybe Minute, temperature :: Maybe Temperature, emulsion :: Emulsion }
  | WASH { solution :: S.Solution, emulsion :: Emulsion }
  | DIGESTION { solution :: S.Solution, duration :: Maybe Minute, temperature :: Maybe Temperature, emulsion :: Emulsion }
  deriving Show

stage :: S.Step -> State Emulsion ()
stage (S.TEMPERATURE newTemp) = do  -- Deal with setting the temperature
  currentEmulsion <- get
  put (case currentEmulsion of
            (WASH s e) -> DIGESTION { solution = s, duration = Nothing, temperature = Just newTemp, emulsion = e}
            other -> other { temperature = Just newTemp })
  return ()
stage (S.REST minutes) = do  -- Deal with setting the duration
  currentEmulsion <- get
  put (case currentEmulsion of
            (WASH _ _) -> currentEmulsion
            other -> other {duration = Just minutes})
  return ()
stage S.WASH = do  -- Deal with entering wash stage
  currentEmulsion <- get
  put (case currentEmulsion of
            (WASH s e) -> WASH {solution = s, emulsion = e}
            (DIGESTION s _ _ e) -> WASH {solution = s, emulsion = e}
            (DOUBLEJET r g _ _ e) -> WASH {solution = foldl S.addSolutions r (map S.solution g), emulsion = e}
            other -> WASH {solution = S.addSolutions (receivingSolution other) (S.solution $ givingSolution other), emulsion = emulsion other})
  return ()
stage (S.ADDITION pours) = do  -- The most complicated logic
  currentEmulsion <- get
  put currentEmulsion
  return ()


-- type Transition = S.Solution -> S.Step -> State Emulsion S.Solution

-- transitions :: Foldable f => Transition -> S.Solution -> f S.Step -> State Emulsion S.Solution
-- foldRecipe transitioner sol steps = do
--   State START
--   foldM transitioner sol steps

-- transition :: S.Solution -> S.Step -> State Emulsion S.Solution
-- transition currentEmulsion (S.TEMPERATURE newTemp) = case currentEmulsion of b@PRECIPITATION{} -> b{temperature=Just newTemp}
--                                                                              b@DIGESTION{} -> b{temperature=Just newTemp}
--                                                                              b@(WASH initialSolution additions emulsion) -> DIGESTION{duration=Nothing, temperature = Just newTemp, additions=[], initialSolution=foldl S.addSolutions initialSolution (map S.solution additions), emulsion=b}
--                                                                              other -> other
-- transition currentEmulsion (S.REST minutes) = case currentEmulsion of b@(PRECIPITATION d t a i e) -> b{duration = Just $ fromMaybe 0.0 d + minutes}
--                                                                              b@(DIGESTION d t a i e) -> b{duration = Just $ fromMaybe 0.0 d + minutes}
--                                                                              other -> other
-- transition b@(WASH i a e) S.WASH = b -- fix
-- transition b S.WASH = WASH{additions=[], initialSolution=foldl S.addSolutions (initialSolution b) (map S.solution (additions b)), emulsion=b}
-- transition b@(START i t a) (S.ADDITION add) = if detectPrecipitation mixed add
--                                                      then PRECIPITATION{duration=Nothing, temperature=Nothing, additions=add, initialSolution=foldl S.addSolutions i previousAdditionSolutions, emulsion=b}
--                                                      else b{additions=a ++ add}
--                                                      where previousAdditionSolutions = map S.solution a
--                                                            mixed = foldl S.addSolutions i previousAdditionSolutions
-- transition b (S.ADDITION add) = if detectPrecipitation mixed add
--                                        then PRECIPITATION{duration=Nothing, temperature=Nothing, additions=add, initialSolution=foldl S.addSolutions i previousAdditionSolutions, emulsion=b}
--                                        else b{additions=a ++ add, emulsion=b}
--                                        where previousAdditionSolutions = map S.solution (additions b)
--                                              i = initialSolution b
--                                              a = additions b
--                                              mixed = foldl S.addSolutions i previousAdditionSolutions

-- detectPrecipitation :: S.Solution -> [S.Pour] -> Bool
-- detectPrecipitation oldMixedSolution unmixed = fst $ foldl precipitationReducer (False, oldMixedSolution) newAdditions
--     where
--     newAdditions = map S.solution unmixed
--     precipitationReducer :: (Bool, S.Solution) -> S.Solution -> (Bool, S.Solution)
--     precipitationReducer (True, a) b = (True, S.addSolutions a b)
--     precipitationReducer (False, a) b = (detected, combined)
--                     where detected = precipitationTransition a b
--                           combined = S.addSolutions a b
--                           precipitationTransition :: S.Solution -> S.Solution -> Bool
--                           precipitationTransition (S.SOLUTION _ (Just (N.SILVERNITRATE nitrateGrams ) ) _ _ _ _ _) (S.SOLUTION (Just sz) _ _ _ _ _ _)
--                             | nitrateGrams > 0 && not (null sz) = True
--                             | otherwise = False
--                           precipitationTransition (S.SOLUTION (Just sz) _ _ _ _ _ _) (S.SOLUTION _ (Just (N.SILVERNITRATE nitrateGrams ) ) _ _ _ _ _)
--                             | nitrateGrams > 0 && not (null sz) = True
--                             | otherwise = False
--                           precipitationTransition _ _ = False

-- Question one. What stages does this emulsion go through?
-- How many precipitations does it have, is there a wash, is there more than one precipitation, is there digestion, what kind of precipitation?

-- Given an emulsion, give me the precipitations
-- precipitations :: Emulsion S.Solution -> [Stage]

-- Question two. What properties does it have w.r.t. stage?
-- What is the gelatin content during precipitation? Is there additional make up gelatin?
-- What does the final species look like? What is the pH during precipitation?

-- IO
-- analysis :: Emulsion S.Solution -> Writer String (Emulsion S.Solution)