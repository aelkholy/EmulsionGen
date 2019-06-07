{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module Ingredients.Salt (
  Salt(..),
  mergeSalts, mergeSalt, prettySalts
) where

import GHC.Generics
import Data.Aeson
import Data.Maybe (catMaybes)
import Control.Monad
-- Home team
import Ingredients.Ingredient (Chemical(..))

data Salt = KI {gramAmount :: Double}
  | KBr {gramAmount :: Double}
  | NaBr {gramAmount :: Double}
  | NaCl {gramAmount :: Double}
   deriving (Generic, Show, Eq, Ord, ToJSON, FromJSON)

instance Chemical Salt where
  molecularWeight (KI _) = 166.0028
  molecularWeight (KBr _) = 119.002
  molecularWeight (NaBr _) = 102.894
  molecularWeight (NaCl _) = 58.44
  grams = gramAmount

prettySalt :: Salt -> String
prettySalt (KI s) = unwords ["Potassium Iodine" , show s, "grams"]
prettySalt (KBr s) = unwords ["Potassium Bromide" , show s, "grams"]
prettySalt (NaBr s) = unwords ["Sodium Bromide" , show s, "grams"]
prettySalt (NaCl s) = unwords ["Sodium Chloride" , show s, "grams"]

prettySalts :: [Salt] -> [String]
prettySalts = map prettySalt

mergeSalt :: Salt -> Salt -> Maybe Salt
mergeSalt (KI x) (KI y) = Just KI{gramAmount = x + y}
mergeSalt (KBr x) (KBr y) = Just KBr{gramAmount = x + y}
mergeSalt (NaBr x) (NaBr y) = Just NaBr{gramAmount = x + y}
mergeSalt (NaCl x) (NaCl y) = Just NaCl{gramAmount = x + y}
mergeSalt _ _ = Nothing

mergeSalts :: [Salt] -> [Salt]
mergeSalts comb = let
  ki = [ x | x@(KI _) <- comb ]
  kbr = [ x | x@(KBr _) <- comb ]
  nbr = [ x | x@(NaBr _) <- comb ]
  na = [ x | x@(NaCl _) <- comb ]
  ki_merged = if length ki > 1 then foldM mergeSalt (KI{gramAmount=0.0}) ki else if length ki == 1 then Just $ head ki else Nothing
  kbr_merged = if length kbr > 1 then foldM mergeSalt (KBr{gramAmount=0.0}) kbr else if length kbr == 1 then Just $ head kbr else Nothing
  nbr_merged = if length nbr > 1 then foldM mergeSalt (NaBr{gramAmount=0.0}) nbr else if length nbr == 1 then Just $ head nbr else Nothing
  na_merged = if length na > 1 then foldM mergeSalt (NaCl{gramAmount=0.0}) na else if length na == 1 then Just $ head na else Nothing
  in catMaybes [ki_merged, kbr_merged, nbr_merged, na_merged]