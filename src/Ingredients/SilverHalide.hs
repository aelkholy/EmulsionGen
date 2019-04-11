{-# LANGUAGE DeriveGeneric #-}
module Ingredients.SilverHalide (
  SilverHalide(..), mergeHalides
) where

import GHC.Generics
import Data.Maybe

import Ingredients.Basics (Chemical(..))

data SilverHalide = AgI { moles :: Double }
  | AgBr { moles :: Double }
  | AgCl { moles :: Double } deriving (Generic, Show)

instance Chemical SilverHalide where
  molecularWeight (AgI _)  = 234.77
  molecularWeight (AgBr _)  = 187.77
  molecularWeight (AgCl _)  = 143.32

mergeHalide :: SilverHalide -> SilverHalide -> Maybe SilverHalide
mergeHalide (AgI x) (AgI y) = Just AgI{moles = x + y}
mergeHalide (AgBr x) (AgBr y) = Just AgBr{moles = x + y}
mergeHalide (AgCl x) (AgCl y) = Just AgCl{moles = x + y}
mergeHalide _ _ = Nothing

mergeHalides :: [SilverHalide] -> [SilverHalide] -> [SilverHalide]
mergeHalides halidesOne halidesTwo = catMaybes [ mergeHalide x y | x <- halidesOne, y <- halidesTwo]