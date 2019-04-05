{-# LANGUAGE DeriveGeneric #-}
module SilverGelatin ( 
  Emulsion, Transition,
  Solution
  ) where

import GHC.Generics
import Control.Monad

import Ingredients.Basics (Quantity, Time, Temperature, Rate)
import Ingredients.Silver (Silver)
import Ingredients.Salt (Salt(..))
import Ingredients.ChemicalModifier (ChemicalModifier)

-- http://www.tcs.hut.fi/Studies/T-79.186/2004/lecture3.pdf
-- Let AP be a non-empty set of atomic propositions A Kripke structure is a tuple M = (S,s^0,R,L), Where
--  S is a finite set of states
--  s^0 in S is an initial state
--  R:S X S: is a transition relation, for which it holds that for all s in S: there exists s` in S:(s,s`) in R and
--  L:S->2^(AP): is labeling, a function which labels each state with the atomic propositions which hold in that state.

-- A path pi in a Kripke structure M = (S,s^0,R,L) is an infinite sequence of states,
-- pi=s0 s1 s2..., such that s0 in I and T(si, si+1)for all i>=0.

-- States are emulsion making steps
-- Initial states are unmixed states
-- transitions are mixing things
-- labelling function is the properties (propositions) at each state. Temperature, reactant quantities, etc.

-- Emulsion Definitions --

type Acid = ChemicalModifier

data Solution = SOLUTION {
  salts :: Maybe [Salt],
  ag :: Maybe Silver,
  acid :: Maybe Acid,
  other :: Maybe [ChemicalModifier],
  water :: Maybe Double,
  temp :: Maybe Temperature
} deriving (Generic, Show)

data Transition = TRANSITION { addition :: Solution, rate :: Maybe Rate, waitTimeAfter :: Time} deriving (Generic, Show)

data Emulsion = EMULSION {
  initialSolution :: Solution,
  transitions :: [Transition]
} deriving (Generic, Show)