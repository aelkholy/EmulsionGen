{-# LANGUAGE DeriveGeneric #-}
module Ingredients.Salt (
  Salt
) where

import GHC.Generics

import Ingredients.Basics (Quantity, Chemical(..))

data Salt = KI { quantity :: Quantity }
  | KBr { quantity :: Quantity }
  | NaCl { quantity :: Quantity } deriving (Generic, Show)

instance Chemical Salt where
  molecularWeight (KI _) = Just 166.0028
  molecularWeight (KBr _) = Just 119.002
  molecularWeight (NaCl _) = Just 58.44