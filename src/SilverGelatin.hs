{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
module SilverGelatin (
  Solution(..), Step(..),
  foldEmulsion, reducer, Mix, 
  saltReaction
  -- withLogging, MixIO, runEmulsion
  ) where

-- Hackage
import GHC.Generics
import Control.Monad                (foldM)
import Data.List                    (sort, unfoldr)
-- Homies
import Ingredients.Basics           (Time, Temperature, Rate, Chemical(..))
import Ingredients.SilverNitrate    (SilverNitrate(..))
import Ingredients.SilverHalide     (SilverHalide(..), mergeHalides)
import Ingredients.Salt             (Salt(..), mergeSalts)
import Ingredients.ChemicalModifier  (ChemicalModifier)
import Text.Printf                  (printf)

-- EMULSIONS

type Ph = ChemicalModifier

mergePH :: Maybe Ph -> Maybe Ph -> Maybe Ph
mergePH (Just phOne) (Just newPH) = Nothing
mergePH Nothing (Just newPH) = Just newPH
mergePH (Just phOne) Nothing = Just phOne

data Solution = SOLUTION {
  salts :: [Salt],
  agnox :: SilverNitrate,
  agH :: [SilverHalide],
  ph :: Maybe Ph,
  other :: [ChemicalModifier],
  water :: Double,
  temp :: Maybe Temperature,
  resting :: Double
} deriving (Generic, Show)

data Step = TEMPERATURE {temperature :: Double}
 | ADDITION {solution :: Solution, rate :: Rate}
 | REST {time :: Double}
 | PH {newPh ::Ph}
 | STOP deriving (Generic, Show)

-- Pass foldable seq of events
type Mix state step = state -> step -> state
foldEmulsion :: Foldable f => Mix state step -> state -> f step -> state
foldEmulsion = foldl

-- CHEMICAL REACTIONS

reducer :: Solution -> Step -> Solution
reducer soln STOP = soln
reducer soln (TEMPERATURE newTemp) = soln {temp = Just newTemp}
reducer soln (REST time) = soln {resting = time}
reducer soln (PH newPH) = soln {ph = mergePH (ph soln) (Just newPH), resting=0}
reducer soln (ADDITION newSoln _) = 
                          let newSalts = sort $ mergeSalts $ salts soln ++ salts newSoln
                              newAgnox = (SILVERNITRATE $ Ingredients.SilverNitrate.amount (agnox soln) + Ingredients.SilverNitrate.amount (agnox newSoln))
                              reaction = saltsToHalides (newAgnox, newSalts) 
                              reactedNitrate = map fst reaction
                              leftoverNitrate = if not (null reactedNitrate) then last reactedNitrate else SILVERNITRATE 0.0
                              reactedHalides = map snd reaction
                              previousHalides = mergeHalides (agH soln) (agH newSoln)
                              newPh = mergePH (ph soln) (ph newSoln)
                          in SOLUTION {
                            salts = saltReaction newAgnox newSalts,
                            agnox = leftoverNitrate,
                            agH = mergeHalides previousHalides reactedHalides,
                            ph = newPh,
                            other = other soln ++ other newSoln,
                            water = water soln + water newSoln,
                            temp = if temp soln == temp newSoln then temp soln else Nothing,
                            resting = 0.0
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
          a@(NaCl amt) -> AgCl $ reactForAmountB nitrate a

saltReaction :: SilverNitrate -> [Salt] -> [Salt]
saltReaction nitrate [] = []
saltReaction nitrate (x:xs) = leftoverSalt : saltReaction leftoverNitrate xs
  where leftoverSalt = case x of 
          a@(KI amt) -> KI $ reactForLeftoverA a nitrate
          a@(KBr amt) -> KBr $ reactForLeftoverA a nitrate
          a@(NaCl amt) -> NaCl $ reactForLeftoverA a nitrate
        leftoverNitrate = SILVERNITRATE (reactForLeftoverA nitrate x)
