{-# LANGUAGE DeriveGeneric #-}
module Ingredients.Salt (
  Salt(..),
  mergeSalts, mergeSalt, prettySalt
) where

import GHC.Generics
import Data.Maybe (catMaybes)
import Control.Monad
-- Home team
import Ingredients.Ingredient (Chemical(..))

data Salt = KI {amount :: Double}
  | KBr {amount :: Double}
  | NaCl {amount :: Double} deriving (Generic, Show, Eq, Ord)

instance Chemical Salt where
  molecularWeight (KI _) = 166.0028
  molecularWeight (KBr _) = 119.002
  molecularWeight (NaCl _) = 58.44
  grams = amount

prettySalt :: Salt -> String
prettySalt (KI s) = unwords ["Potassium Iodine" , show s, "grams"]
prettySalt (KBr s) = unwords ["Potassium Bromide" , show s, "grams"]
prettySalt (NaCl s) = unwords ["Sodium Chloride" , show s, "grams"]

mergeSalt :: Salt -> Salt -> Maybe Salt
mergeSalt (KI x) (KI y) = Just KI{amount = x + y}
mergeSalt (KBr x) (KBr y) = Just KBr{amount = x + y}
mergeSalt (NaCl x) (NaCl y) = Just NaCl{amount = x + y}
mergeSalt _ _ = Nothing

mergeSalts :: [Salt] -> [Salt]
mergeSalts comb = let
  ki = [ x | x@(KI _) <- comb ]
  br = [ x | x@(KBr _) <- comb ]
  na = [ x | x@(NaCl _) <- comb ]
  ki_merged = if length ki > 1 then foldM mergeSalt (KI{amount=0.0}) ki else if length ki == 1 then Just $ head ki else Nothing
  br_merged = if length br > 1 then foldM mergeSalt (KBr{amount=0.0}) br else if length br == 1 then Just $ head br else Nothing
  na_merged = if length na > 1 then foldM mergeSalt (NaCl{amount=0.0}) na else if length na == 1 then Just $ head na else Nothing
  in catMaybes (ki_merged : br_merged : na_merged : [])