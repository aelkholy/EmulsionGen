{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
{-# LANGUAGE LambdaCase #-}
module Solution (
    Solution(..)
  , saltReaction
  , addSolutions
  , washSolution
  , prettySolution
  ) where

-- Hackage
import Data.Maybe
import GHC.Generics
import Data.Aeson
import Data.List                    (unfoldr, nub)
-- Homies
import Physics
import Ingredients.Ingredient       (Chemical(..))
import Ingredients.SilverNitrate    (SilverNitrate(..), prettyNitrate, mergeNitrate)
import Ingredients.SilverHalide     (SilverHalide(..), mergeHalides, prettyHalides, prettyHalideRatio)
import Ingredients.ChemicalModifier  (ChemicalModifier, prettyChemicals)
import Ingredients.Salt             (Salt(..), mergeSalts, prettySalts)

-- Solutions you can make

data Solution = SOLUTION {
  salts :: Maybe [Salt],
  silverNitrate :: Maybe SilverNitrate,
  gramsGelatin :: Maybe Double,
  otherChemicals :: Maybe [ChemicalModifier],
  water :: Maybe Double,
  silverHalides :: Maybe [SilverHalide],
  pH :: Maybe (ChemicalModifier, Maybe PowerHydrogen)
} deriving (Show, Eq, Generic, ToJSON, FromJSON)

prettySolution :: Solution -> String
prettySolution s = 
    let salty = fmap (unwords . prettySalts) (salts s)
        nitrate = fmap prettyNitrate (silverNitrate s)
        gelatin = fmap (\x -> show x ++ " grams gelatin") (gramsGelatin s)
        others = fmap (unwords . prettyChemicals) (otherChemicals s)
        dihydrogenMonoxide = fmap (("-In " ++) . (\x -> show x ++ "ml water")) (water s)
        halides = fmap (unwords . prettyHalides) (silverHalides s)
        ph = fmap (show . snd) (pH s) 
    in unlines $ map ("-" ++) (catMaybes [salty, nitrate, gelatin, others, dihydrogenMonoxide, halides, ph])

-- Reactions

addSolutions :: Solution -> Solution -> Solution
addSolutions soln newSoln = 
  let
    totalSaltsInSolutionBefore = mergeSalts $ concat $ catMaybes [salts soln, salts newSoln]
    totalNitrateInSolution = mergeNitrate (silverNitrate soln) (silverNitrate newSoln)
    reaction = saltsToHalides (totalNitrateInSolution, totalSaltsInSolutionBefore)
    leftoverNitrate = if not . null $ reaction then last $ map fst reaction else totalNitrateInSolution
    generatedHalides = map snd reaction
    previousHalides = mergeHalides (fromMaybe [] (silverHalides soln)) ++ fromMaybe [] (silverHalides newSoln)
  in SOLUTION {
    salts = Just $ saltReaction totalNitrateInSolution totalSaltsInSolutionBefore,
    silverNitrate = Just leftoverNitrate,
    gramsGelatin = Just $ sum . catMaybes $ [gramsGelatin soln, gramsGelatin newSoln],
    silverHalides = Just $ mergeHalides $ previousHalides ++ generatedHalides,
    otherChemicals = Just $ nub $ concat $ catMaybes [otherChemicals soln, otherChemicals newSoln],
    water = Just $ sum . catMaybes $ [water soln, water newSoln],
    pH = pH soln
  }

saltsToHalides :: (SilverNitrate, [Salt]) -> [(SilverNitrate, SilverHalide)]
saltsToHalides (sn, ss) = unfoldr reactor (sn, ss)

reactor :: (SilverNitrate, [Salt]) -> Maybe ((SilverNitrate, SilverHalide), (SilverNitrate, [Salt]))
reactor (nitrate, []) = Nothing
reactor (nitrate, s:alts) = Just((newNitrate, newHalide s), (newNitrate, alts))
  where newNitrate = SILVERNITRATE $ reactForLeftoverA nitrate s
        newHalide = \case
          a@(KI amt) -> AgI $ reactForAmountB nitrate a
          a@(KBr amt) -> AgBr $ reactForAmountB nitrate a
          a@(NaBr amt) -> AgBr $ reactForAmountB nitrate a
          a@(KCl amt) -> AgCl $ reactForAmountB nitrate a
          a@(NaCl amt) -> AgCl $ reactForAmountB nitrate a

saltReaction :: SilverNitrate -> [Salt] -> [Salt]
saltReaction nitrate [] = []
saltReaction nitrate (x:xs) = leftoverSalt : saltReaction leftoverNitrate xs
  where leftoverSalt = case x of 
          a@(KI amt) -> KI $ reactForLeftoverA a nitrate
          a@(KBr amt) -> KBr $ reactForLeftoverA a nitrate
          a@(NaBr amt) -> KBr $ reactForLeftoverA a nitrate
          a@(KCl amt) -> KCl $ reactForLeftoverA a nitrate
          a@(NaCl amt) -> NaCl $ reactForLeftoverA a nitrate
        leftoverNitrate = SILVERNITRATE (reactForLeftoverA nitrate x)


washSolution :: Solution -> Solution
washSolution (SOLUTION salts nitrate gelatin others water halides ph) = SOLUTION {
  salts = Just [],
  silverNitrate = Just SILVERNITRATE {Ingredients.SilverNitrate.gramAmount = 0.0},
  gramsGelatin = gelatin,
  otherChemicals = others,
  water = water,
  silverHalides = halides,
  pH = Nothing
}