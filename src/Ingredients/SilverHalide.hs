{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
module Ingredients.SilverHalide (
  SilverHalide(..), mergeHalides,
  prettyHalides, prettyHalideRatio
) where

-- Away
import GHC.Generics
import Data.Maybe
import Control.Monad          (foldM)
-- Home Team
import Ingredients.Ingredient (Chemical(..))

data SilverHalide = AgI { amountMoles :: Double }
  | AgBr { amountMoles :: Double }
  | AgCl { amountMoles :: Double } deriving (Generic, Show)

instance Chemical SilverHalide where
  molecularWeight (AgI _)  = 234.77
  molecularWeight (AgBr _)  = 187.77
  molecularWeight (AgCl _)  = 143.32
  grams = \case
          a@(AgI amountMoles) -> molesToGrams a amountMoles
          a@(AgBr amountMoles) -> molesToGrams a amountMoles
          a@(AgCl amountMoles) -> molesToGrams a amountMoles
  moles = \case
              (AgI amountMoles) -> amountMoles
              (AgBr amountMoles) -> amountMoles
              (AgCl amountMoles) -> amountMoles
  compareQuantity f a = f (moles a)

mergeHalide :: SilverHalide -> SilverHalide -> Maybe SilverHalide
mergeHalide (AgI x) (AgI y) = Just AgI{amountMoles = x + y}
mergeHalide (AgBr x) (AgBr y) = Just AgBr{amountMoles = x + y}
mergeHalide (AgCl x) (AgCl y) = Just AgCl{amountMoles = x + y}
mergeHalide _ _ = Nothing

mergeHalides :: [SilverHalide] -> [SilverHalide]
mergeHalides comb = let
  ai = [ x | x@(AgI _) <- comb ]
  abr = [ x | x@(AgBr _) <- comb ]
  acl = [ x | x@(AgCl _) <- comb ]
  ai_merged = if length ai > 1 then foldM mergeHalide (AgI{amountMoles=0.0}) ai else if length ai == 1 then Just $ head ai else Nothing
  abr_merged = if length abr > 1 then foldM mergeHalide (AgBr{amountMoles=0.0}) abr else if length abr == 1 then Just $ head abr else Nothing
  acl_merged = if length acl > 1 then foldM mergeHalide (AgCl{amountMoles=0.0}) acl else if length acl == 1 then Just $ head acl else Nothing
  in catMaybes (ai_merged : abr_merged : acl_merged : [])

prettyHalide :: SilverHalide -> String
prettyHalide (AgI x) = unwords ["Silver Iodide:", show x, "moles"]
prettyHalide (AgBr x) = unwords ["Silver Bromine:", show x, "moles"]
prettyHalide (AgCl x) = unwords ["Silver Chloride:", show x, "moles"]

prettyHalides :: [SilverHalide] -> [String]
prettyHalides shl = map prettyHalide shl

halideRatio :: SilverHalide -> [SilverHalide] -> Double
halideRatio a b = moles a / sum ( map moles b )

prettyHalideRatio :: SilverHalide -> [SilverHalide] -> String
prettyHalideRatio h@(AgI x) listhalide = unwords ["Silver Iodide:", show $ 100 * halideRatio h listhalide, "%"]
prettyHalideRatio h@(AgBr x) listhalide = unwords ["Silver Bromine:", show $ 100 * halideRatio h listhalide, "%"]
prettyHalideRatio h@(AgCl x) listhalide = unwords ["Silver Chloride:", show $ 100 * halideRatio h listhalide, "%"]