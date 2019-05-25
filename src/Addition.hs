{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module Addition (
  Addition(..)
  , prettyAddition
) where

import GHC.Generics
import Data.Aeson
-- Home team
import Physics
import qualified Solution as S

data Addition = ADDITION {solution :: S.Solution, rate :: Rate} deriving (Generic, Show, ToJSON, FromJSON)

prettyAddition :: Addition -> String
prettyAddition (ADDITION s r) = unwords [S.prettySolution s, "--AT RATE", prettyRate r]