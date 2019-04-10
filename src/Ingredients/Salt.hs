{-# LANGUAGE DeriveGeneric #-}
module Ingredients.Salt (
  Salt(..)
) where

import GHC.Generics

import Ingredients.Basics (Chemical(..))

data Salt = KI {amount :: Double}
  | KBr {amount :: Double}
  | NaCl {amount :: Double} deriving (Generic, Show, Eq, Ord)

instance Chemical Salt where
  molecularWeight (KI _) = 166.0028
  molecularWeight (KBr _) = 119.002
  molecularWeight (NaCl _) = 58.44
