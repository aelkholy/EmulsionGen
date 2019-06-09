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
  | KCl {gramAmount :: Double}
  | NaCl {gramAmount :: Double}
   deriving (Generic, Show, Eq, Ord, ToJSON, FromJSON)

instance Chemical Salt where
  molecularWeight (KI _) = 166.0028
  molecularWeight (KBr _) = 119.002
  molecularWeight (NaBr _) = 102.894
  molecularWeight (KCl _) = 74.5513
  molecularWeight (NaCl _) = 58.44
  grams = gramAmount

prettySalt :: Salt -> String
prettySalt (KI s) = unwords ["Potassium Iodine" , show s, "grams"]
prettySalt (KBr s) = unwords ["Potassium Bromide" , show s, "grams"]
prettySalt (NaBr s) = unwords ["Sodium Bromide" , show s, "grams"]
prettySalt (KCl s) = unwords ["Potassium Chloride" , show s, "grams"]
prettySalt (NaCl s) = unwords ["Sodium Chloride" , show s, "grams"]

prettySalts :: [Salt] -> [String]
prettySalts = map prettySalt

mergeSalt :: Salt -> Salt -> Maybe Salt
mergeSalt (KI x) (KI y) = Just KI{gramAmount = x + y}
mergeSalt (KBr x) (KBr y) = Just KBr{gramAmount = x + y}
mergeSalt (NaBr x) (NaBr y) = Just NaBr{gramAmount = x + y}
mergeSalt (KCl x) (KCl y) = Just KCl{gramAmount = x + y}
mergeSalt (NaCl x) (NaCl y) = Just NaCl{gramAmount = x + y}
mergeSalt _ _ = Nothing

mergeSalts :: [Salt] -> [Salt]
mergeSalts comb = let
  ki = [ x | x@(KI _) <- comb ]
  kbr = [ x | x@(KBr _) <- comb ]
  nbr = [ x | x@(NaBr _) <- comb ]
  kcl = [ x | x@(KCl _) <- comb ]
  na = [ x | x@(NaCl _) <- comb ]
  ki_merged
    | length ki > 1 = foldM mergeSalt (KI{gramAmount=0.0}) ki
    | length ki == 1 = Just $ head ki
    | otherwise = Nothing
  kbr_merged
    | length kbr > 1 = foldM mergeSalt (KBr{gramAmount=0.0}) kbr
    | length kbr == 1 = Just $ head kbr
    | otherwise = Nothing
  nbr_merged
    | length nbr > 1 = foldM mergeSalt (NaBr{gramAmount=0.0}) nbr
    | length nbr == 1 = Just $ head nbr
    | otherwise = Nothing
  kcl_merged
    | length kcl > 1 = foldM mergeSalt (KCl{gramAmount=0.0}) kcl
    | length kcl == 1 = Just $ head kcl
    | otherwise = Nothing
  na_merged
    | length na > 1 = foldM mergeSalt (NaCl{gramAmount=0.0}) na
    | length na == 1 = Just $ head na
    | otherwise = Nothing
  in catMaybes [ki_merged, kbr_merged, nbr_merged, kcl_merged, na_merged]