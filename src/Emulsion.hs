{-# LANGUAGE DeriveFoldable #-}
-- {-# LANGUAGE LambdaCase #-}
module Emulsion (
  Emulsion
  , startEmulsion
) where

import Data.List                         (unfoldr, tails)
import Data.Maybe                        (fromMaybe)
--
import Physics
import qualified Solution                  as S
import qualified Ingredients.SilverNitrate as N
import Ingredients.Ingredient             (Chemical(..))

data Emulsion a = START {initialSolution :: a, temperature :: Maybe Temperature,  additions :: [S.Pour]}
  | PRECIPITATION { duration :: Maybe Minute, temperature :: Maybe Temperature, additions :: [S.Pour], initialSolution :: a, emulsion :: Emulsion a}
  | DIGESTION { duration :: Maybe Minute, temperature :: Maybe Temperature, additions :: [S.Pour], initialSolution :: a, emulsion :: Emulsion a}
  | WASH { initialSolution :: a, additions :: [S.Pour], emulsion :: Emulsion a}
  deriving (Foldable, Show)
  
-- First I'll build the nested structure
startEmulsion :: S.Solution -> [S.Step] -> Emulsion S.Solution
startEmulsion soln = foldl incrementEmulsion (START{initialSolution=soln, temperature=Nothing, additions=[]})

incrementEmulsion :: Emulsion S.Solution -> S.Step -> Emulsion S.Solution
incrementEmulsion currentEmulsion (S.TEMPERATURE newTemp) = case currentEmulsion of b@PRECIPITATION{} -> b{temperature=Just newTemp}
                                                                                    b@DIGESTION{} -> b{temperature=Just newTemp}
                                                                                    b@(WASH initialSolution additions emulsion) -> DIGESTION{duration=Nothing, temperature = Just newTemp, additions=[], initialSolution=foldl S.addSolutions initialSolution (map S.solution additions), emulsion=b}
                                                                                    other -> other
incrementEmulsion currentEmulsion (S.REST minutes) = case currentEmulsion of b@(PRECIPITATION d t a i e) -> b{duration = Just $ fromMaybe 0.0 d + minutes}
                                                                             b@(DIGESTION d t a i e) -> b{duration = Just $ fromMaybe 0.0 d + minutes}
                                                                             other -> other
incrementEmulsion b@(WASH i a e) S.WASH = b -- fix
incrementEmulsion b S.WASH = WASH{additions=[], initialSolution=foldl S.addSolutions (initialSolution b) (map S.solution (additions b)), emulsion=b}
incrementEmulsion b@(START i t a) (S.ADDITION add) = if detectPrecipitation mixed add
                                                     then PRECIPITATION{duration=Nothing, temperature=Nothing, additions=add, initialSolution=foldl S.addSolutions i previousAdditionSolutions, emulsion=b}
                                                     else b{additions=a ++ add}
                                                     where previousAdditionSolutions = map S.solution a
                                                           mixed = foldl S.addSolutions i previousAdditionSolutions
incrementEmulsion b (S.ADDITION add) = if detectPrecipitation mixed add
                                       then PRECIPITATION{duration=Nothing, temperature=Nothing, additions=add, initialSolution=foldl S.addSolutions i previousAdditionSolutions, emulsion=b}
                                       else b{additions=a ++ add, emulsion=b}
                                       where previousAdditionSolutions = map S.solution (additions b)
                                             i = initialSolution b
                                             a = additions b
                                             mixed = foldl S.addSolutions i previousAdditionSolutions

detectPrecipitation :: S.Solution -> [S.Pour] -> Bool
detectPrecipitation oldMixedSolution unmixed = fst $ foldl precipitationReducer (False, oldMixedSolution) newAdditions
    where
    newAdditions = map S.solution unmixed
    precipitationReducer :: (Bool, S.Solution) -> S.Solution -> (Bool, S.Solution)
    precipitationReducer (True, a) b = (True, S.addSolutions a b)
    precipitationReducer (False, a) b = (detected, combined)
                    where detected = precipitationTransition a b
                          combined = S.addSolutions a b
                          precipitationTransition :: S.Solution -> S.Solution -> Bool
                          precipitationTransition (S.SOLUTION _ (Just (N.SILVERNITRATE nitrateGrams ) ) _ _ _ _ _) (S.SOLUTION (Just sz) _ _ _ _ _ _)
                            | nitrateGrams > 0 && not (null sz) = True
                            | otherwise = False
                          precipitationTransition (S.SOLUTION (Just sz) _ _ _ _ _ _) (S.SOLUTION _ (Just (N.SILVERNITRATE nitrateGrams ) ) _ _ _ _ _)
                            | nitrateGrams > 0 && not (null sz) = True
                            | otherwise = False
                          precipitationTransition _ _ = False

-- Question one. What stages does this emulsion go through?
-- How many precipitations does it have, is there a wash, is there more than one precipitation, is there digestion, what kind of precipitation?

-- Given an emulsion, give me the precipitations
-- precipitations :: Emulsion S.Solution -> [Stage]

-- Question two. What properties does it have w.r.t. stage?
-- What is the gelatin content during precipitation? Is there additional make up gelatin?
-- What does the final species look like? What is the pH during precipitation?

-- IO
-- analysis :: Emulsion S.Solution -> Writer String (Emulsion S.Solution)