{-# LANGUAGE DeriveGeneric #-}
module Ingredients.SilverHalide (
  SilverHalide(..), mergeHalides
) where

import GHC.Generics
import Data.Maybe
import Control.Monad      (foldM)
import Ingredients.Basics (Chemical(..))

data SilverHalide = AgI { moles :: Double }
  | AgBr { moles :: Double }
  | AgCl { moles :: Double } deriving (Generic, Show)

instance Chemical SilverHalide where
  molecularWeight (AgI _)  = 234.77
  molecularWeight (AgBr _)  = 187.77
  molecularWeight (AgCl _)  = 143.32

mergeHalide :: SilverHalide -> SilverHalide -> Maybe SilverHalide
mergeHalide (AgI x) (AgI y) = Just AgI{moles = x + y}
mergeHalide (AgBr x) (AgBr y) = Just AgBr{moles = x + y}
mergeHalide (AgCl x) (AgCl y) = Just AgCl{moles = x + y}
mergeHalide _ _ = Nothing

mergeHalides :: [SilverHalide] -> [SilverHalide] -> [SilverHalide]
mergeHalides ones twos = let
  comb = ones ++ twos
  ai = [ x | x@(AgI _) <- comb ]
  abr = [ x | x@(AgBr _) <- comb ]
  acl = [ x | x@(AgCl _) <- comb ]
  ai_merged = if length ai > 1 then foldM mergeHalide (AgI{moles=0.0}) ai else if length ai == 1 then Just $ head ai else Nothing
  abr_merged = if length abr > 1 then foldM mergeHalide (AgBr{moles=0.0}) abr else if length abr == 1 then Just $ head abr else Nothing
  acl_merged = if length acl > 1 then foldM mergeHalide (AgCl{moles=0.0}) acl else if length acl == 1 then Just $ head acl else Nothing
  in catMaybes (ai_merged : abr_merged : acl_merged : [])