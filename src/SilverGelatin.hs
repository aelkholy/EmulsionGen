{-# LANGUAGE DeriveGeneric #-}
module SilverGelatin ( 
  Emulsion(..), Transition,
  Solution(..), reactAGX
  ) where

import GHC.Generics
import Control.Monad.State
import qualified Data.Foldable as F

import Ingredients.Basics (Quantity, Time, Temperature, Rate, molecularWeight)
import Ingredients.SilverNitrate (SilverNitrate(..))
import Ingredients.SilverHalide (SilverHalide(..))
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

type Ph = ChemicalModifier

data Solution = SOLUTION {
  salts :: [Salt],
  agnox :: SilverNitrate,
  agH :: [SilverHalide],
  ph :: Maybe Ph,
  other :: [ChemicalModifier],
  water :: Double,
  temp :: Temperature
} deriving (Generic, Show)

data Transition = TRANSITION { addition :: Solution, rate :: Maybe Rate, waitTimeAfter :: Time} deriving (Generic, Show)

-- data Emulsion = EMULSION {
--   initialSolution :: Solution,
--   transitions :: [Transition]
-- } deriving (Generic, Show)

data Emulsion = Solution | MIXTURES { transitions :: [Transition] } deriving (Generic, Show)

-- Focus
type EmulsionZipper a = ([a],[a])

goForward :: EmulsionZipper a -> EmulsionZipper a  
goForward (x:xs, bs) = (xs, x:bs)  
  
goBack :: EmulsionZipper a -> EmulsionZipper a
goBack (xs, b:bs) = (b:xs, bs)  

-- Fold --

-- Combining functions
-- mix :: Solution -> Solution -> Solution
-- mix one two = 



reactAGX :: [SilverHalide] -> SilverNitrate -> [Salt] -> [SilverHalide]
reactAGX sh sn (x:xs) = case x of 
                          (KI sq) -> reactAGX (sh ++ [AgI {Ingredients.SilverHalide.moles = molesAGX}] ) (SILVERNITRATE {Ingredients.SilverNitrate.amount = leftoverGramsAGN}) xs
                          (KBr sq) -> reactAGX (sh ++ [AgBr {Ingredients.SilverHalide.moles = molesAGX}] ) (SILVERNITRATE {Ingredients.SilverNitrate.amount = leftoverGramsAGN}) xs
                          (NaCl sq) -> reactAGX (sh ++ [AgCl {Ingredients.SilverHalide.moles = molesAGX}] ) (SILVERNITRATE {Ingredients.SilverNitrate.amount = leftoverGramsAGN}) xs
  where mwSilverNitrate = molecularWeight sn;
        molesAGN = (Ingredients.SilverNitrate.amount sn) / (mwSilverNitrate);
        molesX = (Ingredients.Salt.amount x) / (molecularWeight x);
        molesAGX = min molesAGN molesX;
        leftoverGramsAGN = (mwSilverNitrate) * (molesAGN - (min molesAGN molesX))
reactAGX sh sn [] = sh