{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
module Emulsion (
  Solution(..), Step(..), Mix,
  foldEmulsion, followRecipe, 
  saltReaction
  ) where

-- Hackage
import GHC.Generics
import Control.Monad                (foldM)
import Data.List                    (sort, unfoldr)
import Control.Monad.Writer
import Data.Maybe
-- Homies
import Physics                      (Second, Minute, Temperature, Rate, prettyTemperature, prettyRate)
import Ingredients.Ingredient       (Chemical(..))
import Ingredients.SilverNitrate    (SilverNitrate(..), prettyNitrate)
import Ingredients.SilverHalide     (SilverHalide(..), mergeHalides)
import Ingredients.Salt             (Salt(..), mergeSalts, prettySalt)
import Ingredients.ChemicalModifier  (ChemicalModifier, prettyChemical)

-- Finite state machine

type Ph = ChemicalModifier

mergePH :: Maybe Ph -> Maybe Ph -> Maybe Ph
mergePH (Just phOne) (Just newPH) = Nothing
mergePH Nothing (Just newPH) = Just newPH
mergePH (Just phOne) Nothing = Just phOne

data Solution = SOLUTION {
  salts :: [Salt],
  agnox :: SilverNitrate,
  other :: [ChemicalModifier],
  agH :: [SilverHalide],
  ph :: Maybe Ph,
  water :: Double,
  temp :: Temperature,
  currentlyResting :: Minute
} deriving (Generic, Show)

prettySolution :: Solution -> String
prettySolution s = str
    where str = ingredients ++ others
          ingredients = unlines ( map ("-" ++) (map prettySalt (salts s) ++ [prettyNitrate $ agnox s] ++ map prettyChemical (other s)) )
          others = unwords $ ["--In", show $ water s,"milliliters of water"] ++ [prettyTemperature $ temp s]

-- Maybe factor out step
data Step = TEMPERATURE {temperature :: Temperature}
 | ADDITION {solutions :: [(Solution, Rate)]}
 | REST {minutes :: Double}
 | WASH
 | STOP deriving (Generic, Show)

-- Pass foldable seq of events
type Mix = Solution -> Step -> Writer String Solution
foldEmulsion :: Foldable f => Mix -> Solution -> f Step -> Writer String Solution
foldEmulsion f b c = do
  tell $ unlines ["START WITH:", prettySolution b]
  foldM f b c

followRecipe :: Solution -> Step -> Writer String Solution
followRecipe soln STOP = do
  tell "STOP"
  return soln
followRecipe soln (TEMPERATURE newTemp) = do
  tell $ unlines [ unwords ["SET TEMPERATURE TO", prettyTemperature newTemp] ]
  return soln {temp = newTemp}
followRecipe soln (REST minutes) = do
  tell $ unlines [ unwords ["REST FOR", show minutes, "MINUTES"] ]
  return soln {currentlyResting = minutes}
followRecipe soln WASH = do
    tell "WASH EMULSION"
    return soln
followRecipe soln (ADDITION nextSolns) = do
  tell $ unlines (["ADD TO SOLUTION:"] ++ do
      t <- nextSolns
      [prettySolution (fst t), "--At " ++ prettyRate (snd t)]
    )
  return $ foldl mixer soln (map fst nextSolns)


-- Gelatin concentration %
-- Molar concentrations of silver halides
-- Ratio of gelatin to halides
-- % Silver reacted
-- Temperature / time
-- in consideration of phases
-- Guess coating power and amt silver per sq
analyzeRecipe :: Solution -> Step -> Writer String Solution
analyzeRecipe soln STOP = do
  tell ""
  return soln
analyzeRecipe soln (TEMPERATURE newTemp) = do
  tell $ unlines [ unwords ["SET TEMPERATURE TO", prettyTemperature newTemp] ]
  return soln {temp = newTemp}
analyzeRecipe soln (REST minutes) = do
  tell $ unlines [ unwords ["REST FOR", show minutes, "MINUTES"] ]
  return soln {currentlyResting = minutes}
analyzeRecipe soln WASH = do
  tell "Washed (I don't know how to analyze this yet.)"
  return soln
analyzeRecipe soln (ADDITION nextSolns) = do
  tell $ unlines (["ADD TO SOLUTION:"] ++ do
      t <- nextSolns
      [prettySolution (fst t), "--At " ++ prettyRate (snd t)]
    )
  return $ foldl mixer soln (map fst nextSolns)

-- CHEMICAL REACTIONS

mixer :: Solution -> Solution -> Solution
mixer soln newSoln = SOLUTION {
  salts = saltReaction newAgnox newSalts,
  agnox = leftoverNitrate,
  agH = mergeHalides previousHalides reactedHalides,
  ph = newPh,
  other = other soln ++ other newSoln,
  water = water soln + water newSoln,
  temp = if temp soln == temp newSoln then temp soln else Nothing,
  currentlyResting = 0.0
} where newSalts = sort $ mergeSalts $ salts soln ++ salts newSoln
        newAgnox = SILVERNITRATE $ Just (grams (agnox soln) + grams (agnox newSoln) )
        reaction = saltsToHalides (newAgnox, newSalts) 
        reactedNitrate = map fst reaction
        leftoverNitrate = if not (null reactedNitrate) then last reactedNitrate else SILVERNITRATE $ Just 0.0
        reactedHalides = map snd reaction
        previousHalides = mergeHalides (agH soln) (agH newSoln)
        newPh = mergePH (ph soln) (ph newSoln)

saltsToHalides :: (SilverNitrate, [Salt]) -> [(SilverNitrate, SilverHalide)]
saltsToHalides (sn, ss) = unfoldr reactor (sn, ss)

reactor :: (SilverNitrate, [Salt]) -> Maybe ((SilverNitrate, SilverHalide), (SilverNitrate, [Salt]))
reactor (nitrate, []) = Nothing
reactor (nitrate, s:alts) = Just((nitrate, newHalide s), (newNitrate, alts))
  where newNitrate = SILVERNITRATE $ Just $ reactForLeftoverA nitrate s
        newHalide = \case
          a@(KI amt) -> AgI $ reactForAmountB nitrate a
          a@(KBr amt) -> AgBr $ reactForAmountB nitrate a
          a@(NaCl amt) -> AgCl $ reactForAmountB nitrate a

saltReaction :: SilverNitrate -> [Salt] -> [Salt]
saltReaction nitrate [] = []
saltReaction nitrate (x:xs) = leftoverSalt : saltReaction leftoverNitrate xs
  where leftoverSalt = case x of 
          a@(KI amt) -> KI $ reactForLeftoverA a nitrate
          a@(KBr amt) -> KBr $ reactForLeftoverA a nitrate
          a@(NaCl amt) -> NaCl $ reactForLeftoverA a nitrate
        leftoverNitrate = SILVERNITRATE $ Just (reactForLeftoverA nitrate x)
