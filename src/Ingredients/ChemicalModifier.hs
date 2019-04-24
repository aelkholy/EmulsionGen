{-# LANGUAGE DeriveGeneric #-}
module Ingredients.ChemicalModifier (
  ChemicalModifier, prettyChemical
) where

import GHC.Generics
import Data.Maybe
import Control.Applicative

import Ingredients.Basics (Chemical(..), Unit(..), prettyUnit)

data ChemicalModifier = CHEMICALMODIFIER { name :: String, amount :: Double, unit :: Unit, conc :: Maybe Double} deriving (Generic, Show)

prettyChemical :: ChemicalModifier -> String
prettyChemical (CHEMICALMODIFIER name amount unit conc) = unwords $ [name, show amount, prettyUnit unit, unwords $ catMaybes [Just "@ Concentration", fmap show conc, Just "%"::Maybe String]]