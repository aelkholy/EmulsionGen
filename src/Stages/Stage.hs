{-# LANGUAGE DeriveGeneric #-}
module Stages.Stage (
) where

-- Hackage
-- Homies
import Solution                                   as S
import qualified Stages.Digestion                  as D
import qualified Stages.Precipitation              as P
import qualified Ingredients.SilverNitrate  as AG
import Ingredients.ChemicalModifier         (ChemicalModifier)
import Ingredients.Ingredient              (Chemical(..))
import Physics                             (Rate(..), Temperature, Minute)


-- ([Solution] -> Step -> [Solution]) -> [Solution] -> [Step] -> [[Solution]]

-- class Stage a where
--   parseSolutions :: [S.Solution] -> a
--   inferStage :: Solution -> Step -> Solution
--   -- scanPrecipitation :: Solution -> [Step] -> a
--   -- scanPrecipitation stg steps = 
--   --   where scanned = scanl inferStage stg steps

-- instance Stage P.Precipitation where
--   parseSolutions sols = P.SINGLEJET{P.duration=10, P.temperature=Just 10}

