{-# LANGUAGE DeriveGeneric #-}
module Ingredients.Salt (
  Salt(..), mergeSalts
) where

import GHC.Generics
import Data.Maybe (catMaybes)
import Ingredients.Basics (Chemical(..))

data Salt = KI {amount :: Double}
  | KBr {amount :: Double}
  | NaCl {amount :: Double} deriving (Generic, Show, Eq, Ord)

instance Chemical Salt where
  molecularWeight (KI _) = 166.0028
  molecularWeight (KBr _) = 119.002
  molecularWeight (NaCl _) = 58.44

mergeSalt :: Salt -> Salt -> Maybe Salt
mergeSalt (KI x) (KI y) = Just KI{amount = x + y}
mergeSalt (KBr x) (KBr y) = Just KBr{amount = x + y}
mergeSalt (NaCl x) (NaCl y) = Just NaCl{amount = x + y}
mergeSalt _ _ = Nothing

mergeSalts :: [Salt] -> [Salt] -> [Salt]
mergeSalts saltsOne saltsTwo = catMaybes [ mergeSalt x y | x <- saltsOne, y <- saltsTwo]