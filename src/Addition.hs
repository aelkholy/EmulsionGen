{-# LANGUAGE DeriveGeneric #-}
module Addition (
  Addition(..)
  , prettyAddition
) where

import GHC.Generics
-- Home team
import Physics
import qualified Solution as S

data Addition = ADDITION {solution :: S.Solution, rate :: Rate} deriving (Generic, Show)

prettyAddition :: Addition -> String
prettyAddition (ADDITION s r) = unwords [S.prettySolution s, "--AT RATE", prettyRate r]