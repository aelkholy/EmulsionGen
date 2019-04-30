{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
module Ingredients.SilverHalide (
  SilverHalide(..), mergeHalides
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

mergeHalide :: SilverHalide -> SilverHalide -> Maybe SilverHalide
mergeHalide (AgI x) (AgI y) = Just AgI{amountMoles = x + y}
mergeHalide (AgBr x) (AgBr y) = Just AgBr{amountMoles = x + y}
mergeHalide (AgCl x) (AgCl y) = Just AgCl{amountMoles = x + y}
mergeHalide _ _ = Nothing

mergeHalides :: [SilverHalide] -> [SilverHalide] -> [SilverHalide]
mergeHalides ones twos = let
  comb = ones ++ twos
  ai = [ x | x@(AgI _) <- comb ]
  abr = [ x | x@(AgBr _) <- comb ]
  acl = [ x | x@(AgCl _) <- comb ]
  ai_merged = if length ai > 1 then foldM mergeHalide (AgI{amountMoles=0.0}) ai else if length ai == 1 then Just $ head ai else Nothing
  abr_merged = if length abr > 1 then foldM mergeHalide (AgBr{amountMoles=0.0}) abr else if length abr == 1 then Just $ head abr else Nothing
  acl_merged = if length acl > 1 then foldM mergeHalide (AgCl{amountMoles=0.0}) acl else if length acl == 1 then Just $ head acl else Nothing
  in catMaybes (ai_merged : abr_merged : acl_merged : [])