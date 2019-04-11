{-# LANGUAGE DeriveGeneric #-}
module Ingredients.ChemicalModifier (
  ChemicalModifier
) where

import GHC.Generics

import Ingredients.Basics (Chemical(..), Unit(..))

data ChemicalModifier = CHEMICALMODIFIER { name :: String, amount :: Double, unit :: Unit, conc :: Maybe Double} deriving (Generic, Show)