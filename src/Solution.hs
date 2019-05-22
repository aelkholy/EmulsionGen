{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
module Solution (
  Solution(..), Step(..), Mix, Pour(..),
  foldRecipe, followRecipe, 
  saltReaction, mixer, addSolutions
  ) where

-- Hackage
import Data.Maybe
import GHC.Generics
import Control.Monad.Writer
import Control.Monad                (foldM)
import Data.List                    (sort, unfoldr, nub)
-- Homies
import Physics                      (Second, Minute, Temperature, Rate, prettyTemperature, prettyRate)
import Ingredients.Ingredient       (Chemical(..))
import Ingredients.SilverNitrate    (SilverNitrate(..), prettyNitrate, mergeNitrate)
import Ingredients.SilverHalide     (SilverHalide(..), mergeHalides, prettyHalides, prettyHalideRatio)
import Ingredients.Salt             (Salt(..), mergeSalts, prettySalts)
import Ingredients.ChemicalModifier  (ChemicalModifier, prettyChemicals)

-- Solutions you can make

data Solution = SOLUTION {
  salts :: Maybe [Salt],
  silverNitrate :: Maybe SilverNitrate,
  gramsGelatin :: Maybe Double,
  otherChemicals :: Maybe [ChemicalModifier],
  water :: Maybe Double,
  silverHalides :: Maybe [SilverHalide]
} deriving (Generic, Show)

prettySolution :: Solution -> String
prettySolution s = 
    let salty = fmap (unwords . prettySalts) (salts s)
        nitrate = fmap prettyNitrate (silverNitrate s)
        gelatin = fmap (\x -> show x ++ " grams gelatin") (gramsGelatin s)
        others = fmap (unwords . prettyChemicals) (otherChemicals s)
        dihydrogenMonoxide = fmap (("-In " ++) . (\x -> show x ++ "ml water")) (water s)
        halides = fmap (unwords . prettyHalides) (silverHalides s)
    in unlines $ map ("-" ++) (catMaybes [salty, nitrate, gelatin, others, dihydrogenMonoxide, halides])

-- Steps you can take

data Step = TEMPERATURE {newCelsius :: Temperature}
 | ADDITION {additions :: [Pour]}
 | REST {minutes :: Double}
 | WASH deriving (Generic, Show)

data Pour = POUR {solution :: Solution, rate :: Rate} deriving (Generic, Show)

-- Fold over the recipe // state machine

type Mix = Solution -> Step -> Writer String Solution

foldRecipe :: Foldable f => Mix -> Solution -> f Step -> Writer String Solution
foldRecipe f b c = do
  tell $ "START WITH:\n" ++ prettySolution b
  foldM f b c

-- Functions that we can pass into the state machine that we will fold over the recipe

followRecipe :: Solution -> Step -> Writer String Solution
followRecipe soln (TEMPERATURE newTemp) = do
  tell $ unlines [ unwords ["SET TEMPERATURE TO", prettyTemperature newTemp] ]
  return soln
followRecipe soln (REST minutes) = do
  tell $ unlines [ unwords ["REST FOR", show minutes, "MINUTES"] ]
  return soln
followRecipe soln WASH = do
    tell $ unlines ["WASH EMULSION"]
    return soln
followRecipe soln a@(ADDITION nextSolns) = do
  tell $ unlines $ map (
      \x -> "ADD TO SOLUTION:\n" ++ prettySolution (solution x) ++ "--At " ++ prettyRate (rate x)
    ) nextSolns
  return $ mixer soln a

mixer :: Solution -> Step -> Solution
mixer soln (TEMPERATURE t) = soln
mixer soln (REST t) = soln
mixer soln WASH = soln
mixer soln (ADDITION sols) = foldl addSolutions x xs
    where x = solution $ head sols
          xs = map solution $ tail sols

-- Reactions

addSolutions :: Solution -> Solution -> Solution
addSolutions soln newSoln = 
  let
    totalSaltsInSolutionBefore = mergeSalts $ foldl (++) [] $ catMaybes [salts soln, salts newSoln]
    totalNitrateInSolution = mergeNitrate (silverNitrate soln) (silverNitrate newSoln)
    reaction = saltsToHalides (totalNitrateInSolution, totalSaltsInSolutionBefore)
    leftoverNitrate = if not . null $ reaction then last $ map fst reaction else SILVERNITRATE{Ingredients.SilverNitrate.gramAmounts = 0}
    generatedHalides = map snd reaction
    previousHalides = mergeHalides (fromMaybe [] (silverHalides soln)) ++ fromMaybe [] (silverHalides newSoln)
  in SOLUTION {
    salts = Just $ saltReaction totalNitrateInSolution totalSaltsInSolutionBefore,
    silverNitrate = Just leftoverNitrate,
    gramsGelatin = Just $ sum . catMaybes $ [gramsGelatin soln, gramsGelatin newSoln],
    silverHalides = Just $ mergeHalides $ previousHalides ++ generatedHalides,
    otherChemicals = Just $ nub $ foldl (++) [] $ catMaybes [otherChemicals soln, otherChemicals newSoln],
    water = Just $ sum . catMaybes $ [water soln, water newSoln]
  }

saltsToHalides :: (SilverNitrate, [Salt]) -> [(SilverNitrate, SilverHalide)]
saltsToHalides (sn, ss) = unfoldr reactor (sn, ss)

reactor :: (SilverNitrate, [Salt]) -> Maybe ((SilverNitrate, SilverHalide), (SilverNitrate, [Salt]))
reactor (nitrate, []) = Nothing
reactor (nitrate, s:alts) = Just((nitrate, newHalide s), (newNitrate, alts))
  where newNitrate = SILVERNITRATE $ reactForLeftoverA nitrate s
        newHalide = \case
          a@(KI amt) -> AgI $ reactForAmountB nitrate a
          a@(KBr amt) -> AgBr $ reactForAmountB nitrate a
          a@(NaBr amt) -> AgBr $ reactForAmountB nitrate a
          a@(NaCl amt) -> AgCl $ reactForAmountB nitrate a

saltReaction :: SilverNitrate -> [Salt] -> [Salt]
saltReaction nitrate [] = []
saltReaction nitrate (x:xs) = leftoverSalt : saltReaction leftoverNitrate xs
  where leftoverSalt = case x of 
          a@(KI amt) -> KI $ reactForLeftoverA a nitrate
          a@(KBr amt) -> KBr $ reactForLeftoverA a nitrate
          a@(NaBr amt) -> KBr $ reactForLeftoverA a nitrate
          a@(NaCl amt) -> NaCl $ reactForLeftoverA a nitrate
        leftoverNitrate = SILVERNITRATE (reactForLeftoverA nitrate x)
