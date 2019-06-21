{-# LANGUAGE ViewPatterns #-}
module Analysis (
    decodeToProcedure
  , decodeToAnalysis
  , decodeToDebug
  ) where

-- Home team
import Physics
import qualified Step
import qualified State                        as E
import qualified Solution                     as S
import qualified Addition                     as A
import qualified Ingredients.SilverNitrate    as N
import Ingredients.Salt                      (prettySalts)
import Ingredients.Ingredient                (Chemical(..))
import Ingredients.ChemicalModifier           (prettyChemical)
-- FROM STACKAGE
import Data.Aeson                            ( FromJSON, ToJSON, eitherDecode )
import Data.Aeson.Types                      ( Parser )
import Data.Maybe
import Control.Monad
import Control.Monad.Writer
import System.IO  
import System.Environment
import qualified Data.ByteString.Lazy as B

-- Marshal input json functions

decodeToProcedure :: B.ByteString -> Either String (Writer String S.Solution)
decodeToProcedure arg = do
        raw <- inputs
        Right $ uncurry (toProcedure nextStep) raw
        where inputs = eitherDecode arg :: Either String (S.Solution, [Step.Step])

decodeToAnalysis :: B.ByteString -> Either String (Writer String [E.State])
decodeToAnalysis arg = do
        raw <- inputs
        let states = uncurry E.makeStates raw
        Right $ toAnalysis analyzeStates states
        where inputs = eitherDecode arg :: Either String (S.Solution, [Step.Step])

decodeToDebug :: B.ByteString -> Either String (Writer String [E.State])
decodeToDebug arg = do
        raw <- inputs
        let states = uncurry E.makeStates raw
        Right $ do 
          let final = map last $ uncurry E.makeStates raw
          tell $ show final
          return final
        where inputs = eitherDecode arg :: Either String (S.Solution, [Step.Step])

--
-- Question one. What stages does this emulsion go through?
-- How many precipitations does it have,
-- is there a wash,
-- is there more than one precipitation,
-- is there digestion,
-- what kinds of precipitations?
--DONE

-- Questions two. What properties does it have w.r.t. stage?
-- X What is the gelatin content during precipitation?
-- X Ratio gelatin to water
-- X What does the final species look like?
-- X What is the pH during precipitation?
-- X salt / silver ratio
-- X leftover salt
-- Is there additional make up gelatin?
-- Ratio gelatin to salts
-- Coating power

type Analyze = [E.State] -> [E.State] -> Writer String [E.State]

-- This will fold over the states to output the analysis of an emulsion
toAnalysis :: Analyze -> [[E.State]] -> Writer String [E.State]
toAnalysis a states = foldM a firstState (tail states)
  where firstState = head states

analyzeStates :: [E.State] -> [E.State] -> Writer String [E.State]
analyzeStates stateOne stateTwo = do
  tell $ analyzeState stateTwo
  -- normalize state in process
  return stateTwo

analyzeState :: [E.State] -> String
analyzeState (E.NOTHING s a t :xs) = unlines [
      "Nothing"
  ]
analyzeState states@(E.NORMAL{}:_) = unlines $ [
      "Normal precipitation (Silver into salt)",
      unwords ["-Add to solution at rate", prettyRate (A.rate g)]
    ] ++ E.precipitationAnalysis st -- Analysis for all precipitations
      ++ [ -- Analysis for individual precipitations
          unwords ["--Ratio of all salts to silver nitrate", show saltNitrateRatio],
          if saltNitrateRatio > 1 
            then unwords ["---Excess of salt, there will be leftover salts:",
                          show $ filter (\x -> grams x > 0.0) (fromMaybe [] (S.salts finalSolution)),
                          "\n---Will need", show $ molesToGrams N.SILVERNITRATE{N.gramAmount=0} (sum $ filter (>0.0) (maybe [] (map moles) (S.salts finalSolution))),
                          "grams silver nitrate to react leftover salt."
                          ]
            else unwords ["---Excess of nitrate, there will be unreacted nitrate:", show (S.silverNitrate finalSolution)]
      ] -- Final species at the end
      ++ [E.precipitationAnalysisRatios st]
      where st = last states
            g = E.givingSolution st
            finalSolution = E.mixStage st
            saltNitrateRatio = fromJust $ saltToSilver ([A.solution g, E.solution st] ++ E.additionalSolutions st)
analyzeState states@(E.REVERSE{}:_) = unlines $ [
      "Reverse precipitation (Salt into silver)",
      unwords ["-Add to solution at rate", prettyRate (A.rate g)]
    ] ++ E.precipitationAnalysis st -- Analysis for all precipitations
      ++ [ -- Analysis for individual precipitations
          unwords ["--Ratio of all salts to silver nitrate", show saltNitrateRatio],
          if saltNitrateRatio > 1 
            then unwords ["---Excess of salt, there will be leftover salts:",
                          show $ filter (\x -> grams x > 0.0) (fromMaybe [] (S.salts finalSolution)),
                          "\n---Will need", show $ molesToGrams N.SILVERNITRATE{N.gramAmount=0} (sum $ filter (>0.0) (maybe [] (map moles) (S.salts finalSolution))),
                          "grams silver nitrate to react leftover salt."
                          ]
            else unwords ["---Excess of nitrate, there will be unreacted nitrate", show (S.silverNitrate finalSolution)]
      ] -- Final species at the end
      ++ [E.precipitationAnalysisRatios st]
      where st = last states
            g = E.givingSolution st
            finalSolution = E.mixStage st
            saltNitrateRatio = fromJust $ saltToSilver ([A.solution g, E.solution st] ++ E.additionalSolutions st)
analyzeState states@(E.NJET{}:_) = unlines [
      "N-Jet precipitation",
      "The following to be added to solution"
    ] ++ unlines ( map (\x -> 
        unlines [
          fromMaybe "UNKNOWN" $ (++) <$> fmap N.prettyNitrate (S.silverNitrate (A.solution x)) <*> fmap (unwords . prettySalts) (S.salts (A.solution x)),
          unwords ["--Add to solution at rate", prettyRate (A.rate x)]
        ]
      ) g )
      where
      st = last states
      g = E.givingSolutions st
analyzeState states@(E.GENERICWASH{}:_) = unlines [
      "Washed"
    ] where -- was wash needed?
      st = last states
      g = E.givingSolution st
      d = E.duration st
      t = E.temperature st
analyzeState states@(E.DIGESTION{}:_) = unlines [
      "Digestion",
      unwords ["-Digest for", maybe "UNKNOWN DURATION" prettyMinute d],
      unwords ["-At temperature", maybe "UNKNOWN TEMPERATURE" prettyTemperature t]
    ] where
      st = last states
      d = E.duration st
      t = E.temperature st
analyzeState x = unlines [
    show x
  ]

saltToSilver :: [S.Solution] -> Maybe Double
saltToSilver sols
  | silver > 0 = Just $ salts / silver
  | otherwise = Nothing
  where silver = moleSum $ mapMaybe S.silverNitrate sols
        salts = sum $ map moleSum $ mapMaybe S.salts sols

type Procedure = S.Solution -> Step.Step -> Writer String S.Solution

-- This will fold over the solution and steps to output the procedure for making an emulsion
toProcedure :: Foldable f => Procedure -> S.Solution -> f Step.Step -> Writer String S.Solution
toProcedure f b c = do
  tell $ "START WITH:\n" ++ S.prettySolution b
  foldM f b c

-- This will move stepwise through the procedure and output the next step
nextStep :: S.Solution -> Step.Step -> Writer String S.Solution
nextStep soln (Step.TEMPERATURE newTemp) = do
  tell $ unlines [ unwords ["SET TEMPERATURE TO", prettyTemperature newTemp] ]
  return soln
nextStep soln (Step.REST minutes) = do
  tell $ unlines [ unwords ["REST FOR", show minutes, "MINUTES"] ]
  return soln
nextStep soln (Step.STIRRING seconds) = do
  tell $ unlines [ unwords ["STIR FOR", show seconds, "SECONDS"] ]
  return soln
nextStep soln Step.WASH = do
  tell $ unlines ["WASH EMULSION"]
  return $ S.washSolution soln
nextStep soln a@(Step.ADDITION nextSolns) = do
  tell $ unlines $ "ADD" : map A.prettyAddition nextSolns
  return $ Step.moveStep soln a
