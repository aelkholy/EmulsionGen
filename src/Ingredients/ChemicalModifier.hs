{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module Ingredients.ChemicalModifier (
  ChemicalModifier(..)
  , prettyChemical
  , prettyChemicals
) where

import GHC.Generics
import Data.Aeson
import Data.Maybe
import Control.Applicative

import Ingredients.Ingredient (Chemical(..))
import Physics                (Unit(..), prettyUnit)

data ChemicalModifier = CHEMICALMODIFIER { name :: String, medium :: Maybe String, amount :: Double, unit :: Unit, conc :: Maybe Double} deriving (Generic, Show, Eq, ToJSON, FromJSON)

prettyChemical :: ChemicalModifier -> String
prettyChemical (CHEMICALMODIFIER name medium amount unit conc) = unwords [name, "in", maybe "water" show medium, show amount, prettyUnit unit, unwords $ fromMaybe [] (sequence [Just "@ concentration", fmap show conc, Just "%"::Maybe String])]

prettyChemicals :: [ChemicalModifier] -> [String]
prettyChemicals = map prettyChemical
