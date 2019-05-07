module Emulsion (
  Emulsion
) where

--
import qualified Solution             as S
import qualified Stages.Precipitation as P
import qualified Stages.Digestion     as D

-- What questions do I want to ask?
-- What stages does an emulsion go through
-- Because later I want to infer properties and impute based off of these

data Emulsion = EMULSION {precipitation :: [P.Precipitation], digestion :: D.Digestion, finalSolution :: S.Solution}
