{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
{-# LANGUAGE LambdaCase #-}
module Ingredients.SilverHalide (
    SilverHalide(..)
  , mergeHalides
  , prettyHalides
  , prettyHalideRatio
  , prettyHalidesRatio
) where

-- Away
import GHC.Generics
import Data.Aeson
import Data.Maybe
import Control.Monad          (foldM)
-- Home Team
import Ingredients.Ingredient (Chemical(..))

data SilverHalide = AgI { amountMoles :: Double }
  | AgBr { amountMoles :: Double }
  | AgCl { amountMoles :: Double } deriving (Generic, Show, Eq, ToJSON, FromJSON)

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
  ai_merged 
    | length ai > 1 = foldM mergeHalide (AgI{amountMoles=0.0}) ai
    | length ai == 1 = Just $ head ai
    | otherwise = Nothing
  abr_merged 
    | length abr > 1 = foldM mergeHalide (AgBr{amountMoles=0.0}) abr 
    | length abr == 1 = Just $ head abr 
    | otherwise = Nothing
  acl_merged 
    | length acl > 1 = foldM mergeHalide (AgCl{amountMoles=0.0}) acl 
    | length acl == 1 = Just $ head acl 
    | otherwise = Nothing
  in catMaybes [ai_merged, abr_merged, acl_merged]

prettyHalide :: SilverHalide -> String
prettyHalide (AgI x) = unwords ["Silver Iodide:", show x, "moles"]
prettyHalide (AgBr x) = unwords ["Silver Bromine:", show x, "moles"]
prettyHalide (AgCl x) = unwords ["Silver Chloride:", show x, "moles"]

prettyHalides :: [SilverHalide] -> [String]
prettyHalides = map prettyHalide

halideRatio :: SilverHalide -> [SilverHalide] -> Double
halideRatio a b = moles a / sum ( map moles b )

prettyHalideRatio :: SilverHalide -> [SilverHalide] -> String
prettyHalideRatio h@(AgI x) listHalide = unwords ["Silver Iodide:", show $ 100 * halideRatio h listHalide, "%"]
prettyHalideRatio h@(AgBr x) listHalide = unwords ["Silver Bromine:", show $ 100 * halideRatio h listHalide, "%"]
prettyHalideRatio h@(AgCl x) listHalide = unwords ["Silver Chloride:", show $ 100 * halideRatio h listHalide, "%"]

prettyHalidesRatio :: [SilverHalide] -> [String]
prettyHalidesRatio listHalide = map curriedPHR listHalide
  where curriedPHR x = prettyHalideRatio x listHalide