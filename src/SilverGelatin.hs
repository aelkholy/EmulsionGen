{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
module SilverGelatin (
  Solution(..), Step(..),
  foldEmulsion, reducer, Mix, 
  silverReaction, halideReaction, saltReaction
  -- withLogging, MixIO, runEmulsion
  ) where

-- Hackage
import GHC.Generics
import Control.Monad                (foldM)
import Data.List                    (sort)
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

-- type MixIO s e = s -> e -> IO s
-- runEmulsion :: Foldable f => MixIO s e -> s -> f e -> IO s
-- runEmulsion = foldM

-- withLogging :: (Show s, Show e) => MixIO s e -> MixIO s e
-- withLogging emulsion s e = do
--   s' <- emulsion s e
--   printf "- %s x %s -> %s\n" (show s) (show e) (show s')
--   return s'

-- CHEMICAL REACTIONS

reducer :: Solution -> Step -> Solution
reducer soln STOP = soln
reducer soln (TEMPERATURE newTemp) = soln {temp = Just newTemp}
reducer soln (REST time) = soln {resting = time}
reducer soln (PH newPH) = soln {ph = mergePH (ph soln) (Just newPH), resting=0}
reducer soln (ADDITION newSoln _) = 
                          let newSalts = sort $ mergeSalts (salts soln) (salts newSoln)
                              previousAg = Ingredients.SilverNitrate.amount (agnox soln)
                              nextAg = Ingredients.SilverNitrate.amount (agnox newSoln)
                              newAgnox = (SILVERNITRATE $ previousAg + nextAg)
                              newHalides = mergeHalides (agH soln) (agH newSoln)
                              newPh = mergePH (ph soln) (ph newSoln)
                          in SOLUTION {
                            salts = sort $ saltReaction newAgnox newSalts,
                            agnox = silverReaction newAgnox newSalts,
                            agH = mergeHalides newHalides $ halideReaction newAgnox newSalts,
                            ph = newPh,
                            other = other soln ++ other newSoln,
                            water = water soln + water newSoln,
                            temp = if temp soln == temp newSoln then temp soln else Nothing,
                            resting = 0.0
                          }

saltReaction :: SilverNitrate -> [Salt] -> [Salt]
saltReaction sn [] = []
saltReaction sn (x:xs) = saltReaction leftoverNitrate xs ++ [leftoverSalt]
  where leftoverNitrate = SILVERNITRATE $ react sn x
        leftoverSalt = case x of 
          (KI amt) -> KI (amt - react x sn)
          (KBr amt) -> KBr (amt - react x sn)
          (NaCl amt) -> NaCl (amt - react x sn)

silverReaction :: SilverNitrate -> [Salt] -> SilverNitrate
silverReaction sn [] = sn
silverReaction sn (s:ss) = final 
  where firstLost = react sn s
        first = SILVERNITRATE{Ingredients.SilverNitrate.amount=Ingredients.SilverNitrate.amount sn - firstLost}
        final = foldl (\a b->SILVERNITRATE {Ingredients.SilverNitrate.amount=max 0 (Ingredients.SilverNitrate.amount sn - react a b - firstLost)}) first ss

halideReaction :: SilverNitrate -> [Salt] -> [SilverHalide]
halideReaction sn [] = []
halideReaction sn (s:alts) = reactor s : halideReaction (silverReaction sn [s]) alts
  where wat = react sn
        reactor = \case { 
        a@(KI amt) -> AgI $ wat a;
        a@(KBr amt) -> AgBr $ wat a;
        a@(NaCl amt) -> AgCl $ wat a
        }
    
