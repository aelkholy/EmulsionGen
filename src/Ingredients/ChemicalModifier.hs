{-# LANGUAGE DeriveGeneric #-}
module Ingredients.ChemicalModifier (
  ChemicalModifier, prettyChemicals
) where

import GHC.Generics
import Data.Maybe
import Control.Applicative

import Ingredients.Ingredient (Chemical(..))
import Physics                (Unit(..), prettyUnit)

data ChemicalModifier = CHEMICALMODIFIER { name :: String, amount :: Double, unit :: Unit, conc :: Maybe Double} deriving (Generic, Show, Eq)

prettyChemical :: ChemicalModifier -> String
prettyChemical (CHEMICALMODIFIER name amount unit conc) = unwords [name, show amount, prettyUnit unit, unwords $ fromMaybe [] (sequence [Just "@ concentration", fmap show conc, Just "%"::Maybe String])]

prettyChemicals :: [ChemicalModifier] -> [String]
prettyChemicals c = map (prettyChemical) c