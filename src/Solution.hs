{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
module Solution (
  Solution(..), Step(..), Mix,
  foldRecipe, followRecipe, 
  saltReaction
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
  celsius :: Maybe Temperature,
  currentlyResting :: Maybe Minute,
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
 | ADDITION {solutions :: [(Solution, Rate)]}
 | REST {minutes :: Double}
 | WASH
 | STOP deriving (Generic, Show)

-- Fold over the recipe // state machine

type Mix = Solution -> Step -> Writer String Solution

foldRecipe :: Foldable f => Mix -> Solution -> f Step -> Writer String Solution
foldRecipe f b c = do
  tell $ "START WITH:\n" ++ prettySolution b
  foldM f b c

-- Functions that we can pass into the state machine that we will fold over the recipe

followRecipe :: Solution -> Step -> Writer String Solution
followRecipe soln STOP = do
  tell "STOP"
  return soln
followRecipe soln (TEMPERATURE newTemp) = do
  tell $ unlines [ unwords ["SET TEMPERATURE TO", prettyTemperature newTemp] ]
  return soln {celsius = Just newTemp}
followRecipe soln (REST minutes) = do
  tell $ unlines [ unwords ["REST FOR", show minutes, "MINUTES"] ]
  return soln {currentlyResting = Just minutes}
followRecipe soln WASH = do
    tell "WASH EMULSION"
    return soln
followRecipe soln a@(ADDITION nextSolns) = do
  tell $ unlines $ map (
      \x -> "ADD TO SOLUTION:\n" ++ prettySolution (fst x) ++ "--At " ++ prettyRate (snd x)
    ) nextSolns
  return $ mixer soln a

-- Gelatin concentration %
-- x Molar concentrations of silver halides
-- x Ratio of gelatin to halides
-- % Silver reacted
-- Temperature / time
-- in consideration of phases
-- Guess coating power and amt silver per sq
-- analyzeRecipe :: Solution -> Step -> Writer String Solution
-- analyzeRecipe soln STOP = do
--   tell $ unlines $ [
--     "FINAL SOLUTION:",
--     prettySolution soln,
--     "MOLAR CONCENTRATIONS OF SILVER HALIDES:",
--     unwords $ map (\x -> prettyHalideRatio x (fromMaybe [] (silverHalides soln))) (fromMaybe [] (silverHalides soln)),
--     "GELATIN / WATER RATIO:",
--     show $ ratioGelatinWater soln,
--     "GELATIN / HALIDES RATIO:",
--     show $ ratioGelatinHalides soln
--     "ALL HALIDES / GELATIN RATIO:",
--     show $ sum ( ratioHalidesGelatin soln )
--       ] ++ catMaybes [
--         "GELATIN / WATER RATIO:",
--         show $ ratioGelatinWater soln
--           ]
--   return soln
-- analyzeRecipe soln (TEMPERATURE newTemp) = do
--   tell $ unlines [ unwords ["SET TEMPERATURE TO", prettyTemperature newTemp] ]
--   return soln {celsius = newTemp}
-- analyzeRecipe soln (REST minutes) = do
--   tell $ unlines [ unwords ["REST FOR", show minutes, "MINUTES"] ]
--   return soln {currentlyResting = minutes}
-- analyzeRecipe soln WASH = do
--   tell "Washed"
--   return soln
-- analyzeRecipe soln a@(ADDITION nextSolns) = do
--   tell $ unlines (["ADD TO SOLUTION:"] ++ do
--       t <- nextSolns
--       [prettySolution (fst t), "--At " ++ prettyRate (snd t)]
--     )
--   return $ mixer soln a

mixer :: Solution -> Step -> Solution
mixer soln (TEMPERATURE t) = soln {celsius = Just t}
mixer soln (REST t) = soln {currentlyResting = Just t}
mixer soln WASH = soln
mixer soln STOP = soln
mixer soln (ADDITION sols) = foldl addSolutions x xs
    where x = fst $ head sols
          xs = map fst $ tail sols
    

-- addSolutions :: Solution -> Solution -> Solution
-- addSolutions soln newSoln = SOLUTION {
--   salts = saltReaction newsilverNitrate newSalts,
--   silverNitrate = leftoverNitrate,
--   gramsGelatin = Just $ fromMaybe 0.0 (gramsGelatin soln) + fromMaybe 0.0 (gramsGelatin newSoln),
--   silverHalides = Just $ mergeHalides $ previousHalides ++ reactedHalides,
--   ph = newPh,
--   otherChemicals = otherChemicals soln ++ otherChemicals newSoln,
--   water = water soln + water newSoln,
--   celsius = if celsius soln == celsius newSoln then celsius soln else Nothing,
--   currentlyResting = 0.0
-- } where newSalts = sort $ mergeSalts $ salts soln ++ salts newSoln
--         newsilverNitrate = SILVERNITRATE $ Just (grams (silverNitrate soln) + grams (silverNitrate newSoln) )
--         reaction = saltsToHalides (newsilverNitrate, newSalts) 
--         reactedNitrate = map fst reaction
--         leftoverNitrate = if not (null reactedNitrate) then last reactedNitrate else SILVERNITRATE $ Just 0.0
--         reactedHalides = map snd reaction
--         previousHalides = mergeHalides (fromMaybe [] (silverHalides soln)) ++ (fromMaybe [] (silverHalides newSoln))
--         newPh = mergePH (ph soln) (ph newSoln)

-- Reactions

addSolutions :: Solution -> Solution -> Solution
addSolutions soln newSoln = 
  let
    totalSaltsInSolutionBefore = mergeSalts $ foldl (++) [] $ catMaybes [salts soln, salts newSoln]
    totalNitrateInSolution = mergeNitrate (silverNitrate soln) (silverNitrate newSoln)
    reaction = saltsToHalides (totalNitrateInSolution, totalSaltsInSolutionBefore)
    leftoverNitrate = last $ map fst reaction
    generatedHalides = map snd reaction
    previousHalides = mergeHalides (fromMaybe [] (silverHalides soln)) ++ fromMaybe [] (silverHalides newSoln)
  in SOLUTION {
    salts = Just $ saltReaction totalNitrateInSolution totalSaltsInSolutionBefore,
    silverNitrate = Just leftoverNitrate,
    gramsGelatin = Just $ sum . catMaybes $ [gramsGelatin soln, gramsGelatin newSoln],
    silverHalides = Just $ mergeHalides $ previousHalides ++ generatedHalides,
    otherChemicals = Just $ nub $ foldl (++) [] $ catMaybes [otherChemicals soln, otherChemicals newSoln],
    water = Just $ sum . catMaybes $ [water soln, water newSoln],
    celsius = if celsius soln == celsius newSoln then celsius soln else Nothing,
    currentlyResting = Just 0.0
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

-- Analysis

ratioGelatinWater :: Solution -> Maybe Double
ratioGelatinWater sol = (gelatin /) <$> diHydrogenMonoxide where
  gelatin = fromMaybe 0.0 (gramsGelatin sol)
  diHydrogenMonoxide = water sol

ratioHalidesGelatin :: Solution -> [Double]
ratioHalidesGelatin SOLUTION{silverHalides=Just []} = []
ratioHalidesGelatin SOLUTION{silverHalides=Nothing} = []
ratioHalidesGelatin SOLUTION{gramsGelatin=Nothing} = []
ratioHalidesGelatin SOLUTION{silverHalides=(Just listHalide), gramsGelatin = Just gelatin} = map (\x -> grams x / gelatin) listHalide