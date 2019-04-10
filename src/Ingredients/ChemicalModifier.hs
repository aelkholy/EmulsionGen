{-# LANGUAGE DeriveGeneric #-}
module Ingredients.ChemicalModifier (
  ChemicalModifier
) where

import GHC.Generics

import Ingredients.Basics (Quantity(..), Chemical(..), Unit(..))

data ChemicalModifier = CHEMICALMODIFIER { name :: String, quantity :: Quantity} deriving (Generic, Show)