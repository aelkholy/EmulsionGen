module Stages.Stage (
  Stage (..)
) where

-- Hackage
-- Homies
import Emulsion                            (Solution, Step)
import Physics                             (Rate(..), Temperature)
import qualified Stages.Precipitation       as P
import qualified Stages.Digestion           as D

class Stage a where
  detectMe :: Solution -> Step -> a

-- Silver nitrate and salt being mixed results in a precipitation stage.
-- How they're mixed determines what type of precipitation is done.
instance Stage P.Precipitation where
  detectMe sol stp = P.SINGLEJET { P.nitrateSolution=sol, P.rate=RATE{amountAdded=0.0, overTime=0.0}, P.time=0.0, P.temperature=Just 20 }

--if we've washed it and we're heating
instance Stage D.Digestion where
  detectMe sol stp = D.DIGESTION {D.time=0.0, D.temperature=Just 20, D.additives=Nothing}