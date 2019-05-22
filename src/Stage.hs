
module Stage where

import Physics
import qualified Solution       as S
import qualified Emulsion       as E

-- data Stage = NORMAL {receivingSolution :: S.Solution, givingSolution :: S.Pour, duration :: Minute, temperature :: Temperature}
--   | REVERSE {receivingSolution :: S.Solution, givingSolution :: S.Pour, duration :: Minute, temperature :: Temperature}
--   | DOUBLEJET {receivingSolution :: S.Solution, givingSolutions :: [S.Pour], duration :: Minute, temperature :: Temperature}
--   | WASH
--   | DIGESTION {duration :: Minute, temperature :: Temperature} deriving Show


-- Question one. What stages does this emulsion go through?
-- How many precipitations does it have, is there a wash, is there more than one precipitation, is there digestion, what kind of precipitation?
-- In order to do this I'm going to detect transitions

-- emulsionTransitions :: S.Solution -> S.Step -> [Stage]
-- emulsionTransitions currentEmulsion (S.TEMPERATURE newTemp) = case currentEmulsion of b@PRECIPITATION{} -> b{temperature=Just newTemp}
--                                                                                     b@DIGESTION{} -> b{temperature=Just newTemp}
--                                                                                     b@(WASH initialSolution additions emulsion) -> DIGESTION{duration=Nothing, temperature = Just newTemp, additions=[], initialSolution=foldl S.addSolutions initialSolution (map S.solution additions), emulsion=b}
--                                                                                     other -> other
-- emulsionTransitions currentEmulsion (S.REST minutes) = case currentEmulsion of b@(PRECIPITATION d t a i e) -> b{duration = Just $ fromMaybe 0.0 d + minutes}
--                                                                              b@(DIGESTION d t a i e) -> b{duration = Just $ fromMaybe 0.0 d + minutes}
--                                                                              other -> other
-- emulsionTransitions b@(WASH i a e) S.WASH = b -- fix
-- emulsionTransitions b S.WASH = WASH{additions=[], initialSolution=foldl S.addSolutions (initialSolution b) (map S.solution (additions b)), emulsion=b}
-- emulsionTransitions b@(START i t a) (S.ADDITION add) = if detectPrecipitation mixed add
--                                                      then PRECIPITATION{duration=Nothing, temperature=Nothing, additions=add, initialSolution=foldl S.addSolutions i previousAdditionSolutions, emulsion=b}
--                                                      else b{additions=a ++ add}
--                                                      where previousAdditionSolutions = map S.solution a
--                                                            mixed = foldl S.addSolutions i previousAdditionSolutions
-- emulsionTransitions b (S.ADDITION add) = if detectPrecipitation mixed add
--                                        then PRECIPITATION{duration=Nothing, temperature=Nothing, additions=add, initialSolution=foldl S.addSolutions i previousAdditionSolutions, emulsion=b}
--                                        else b{additions=a ++ add, emulsion=b}
--                                        where previousAdditionSolutions = map S.solution (additions b)
--                                              i = initialSolution b
--                                              a = additions b
--                                              mixed = foldl S.addSolutions i previousAdditionSolutions

-- Given an emulsion, give me the precipitations
-- precipitations :: Emulsion S.Solution -> [Stage]

-- Given an emulsion, give me digestions
-- digestions :: Emulsion S.Solution -> [Stage]

-- Question two. What properties does it have w.r.t. stage?
-- What is the gelatin content during precipitation? Is there additional make up gelatin?
-- What does the final species look like? What is the pH during precipitation?

-- IO
-- analysis :: Emulsion S.Solution -> Writer String (Emulsion S.Solution)
