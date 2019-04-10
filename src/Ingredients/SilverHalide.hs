{-# LANGUAGE DeriveGeneric #-}
module Ingredients.SilverHalide (
  SilverHalide(..)
) where

import GHC.Generics

import Ingredients.Basics (Chemical(..))

data SilverHalide = AgI { moles :: Double }
  | AgBr { moles :: Double }
  | AgCl { moles :: Double } deriving (Generic, Show)

instance Chemical SilverHalide where
  molecularWeight (AgI _)  = 234.77
  molecularWeight (AgBr _)  = 187.77
  molecularWeight (AgCl _)  = 143.32
