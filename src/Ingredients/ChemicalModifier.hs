{-# LANGUAGE DeriveGeneric #-}
module Ingredients.ChemicalModifier (
  ChemicalModifier
) where

import GHC.Generics

import Ingredients.Basics (Quantity, Chemical(..))

data ChemicalModifier = CHEMICALMODIFIER { name :: String, quantity :: Quantity, weight :: Maybe Double} deriving (Generic, Show)

instance Chemical ChemicalModifier where
  molecularWeight (CHEMICALMODIFIER _ _ m) = m