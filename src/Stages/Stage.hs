module Stages.Stage (
  Stage (..)
) where

-- Hackage
-- Homies
import qualified Emulsion                   as E
import qualified Ingredients.SilverNitrate  as AG
import Ingredients.Ingredient              (Chemical(..))
import Physics                             (Rate(..), Temperature)
import qualified Stages.Precipitation       as P
import qualified Stages.Digestion           as D

class Stage a where
  detectStage :: E.Solution -> E.Step -> Maybe a

-- Silver nitrate and salt being mixed results in a precipitation stage.
-- How they're mixed determines what type of precipitation is done.
instance Stage P.Precipitation where
  detectStage ss@E.SOLUTION{E.silverNitrate=AG.SILVERNITRATE{AG.gramAmount=Nothing}, E.salts=s:alts} E.ADDITION{E.solutions=[(ns@E.SOLUTION{E.silverNitrate=silver, E.salts=[]},r)]} = Just P.SINGLEJET{P.nitrateSolution=ns, P.saltSolution=ss, P.rate=r, P.restingMinutes=Nothing, P.temperature=Nothing}
  detectStage ns@E.SOLUTION{E.silverNitrate=ag, E.salts=[]} E.ADDITION{E.solutions=[(ss@E.SOLUTION{E.silverNitrate=AG.SILVERNITRATE{AG.gramAmount=Nothing}, E.salts=theSalts},r)]} 
    | grams ag > 0.0 = Just P.REVERSE{P.saltSolution=ss, P.nitrateSolution=ns, P.rate=r, P.restingMinutes=Nothing, P.temperature=Nothing}
  detectStage E.SOLUTION{E.silverNitrate=AG.SILVERNITRATE{AG.gramAmount=Nothing}, E.salts=[]} E.ADDITION{E.solutions=[(ns@E.SOLUTION{E.silverNitrate=silver, E.salts=[]},rN), (ss@E.SOLUTION{E.silverNitrate=AG.SILVERNITRATE{AG.gramAmount=Nothing}, E.salts=theSalts},rS)]} = Just P.DOUBLEJET{P.saltSolution=ss, P.nitrateSolution=ns, P.saltRate=rS, P.nitrateRate=rN, P.restingMinutes=Nothing, P.temperature=Nothing}

--if we've washed it and we're heating
instance Stage D.Digestion where
  detectStage sol stp = Just D.DIGESTION {D.restingMinutes=0.0, D.temperature=Just 20, D.additives=Nothing}