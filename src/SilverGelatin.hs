{-# LANGUAGE DeriveGeneric #-}
module SilverGelatin (
  Solution(..), Step(..),
  foldEmulsion
  ) where

-- Hackage
import GHC.Generics
import Control.Monad                (foldM)
import Data.List                    (sort)
-- Homies
import Ingredients.Basics           (Quantity, Time, Temperature, Rate, molecularWeight)
import Ingredients.SilverNitrate    (SilverNitrate(..))
import Ingredients.SilverHalide     (SilverHalide(..), mergeHalides)
import Ingredients.Salt             (Salt(..), mergeSalts)
import Ingredients.ChemicalModifier (ChemicalModifier)

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

data Step = TEMPERATURE Double
 | ADDITION {solution :: Solution, rate :: Rate}
 | REST Double
 | PH Ph
 | STOP deriving (Generic, Show)

-- Pass foldable seq of events
type Emulsion state step = state -> step -> state
foldEmulsion :: Foldable f => Emulsion state step -> state -> f step -> state
foldEmulsion = foldl

-- runEmulsion :: (Foldable f, Monad m) => Emulsion state step -> state -> f step -> m state
-- runEmulsion = foldM

-- CHEMICAL REACTIONS

reducer :: Solution -> Step -> Solution
reducer soln STOP = soln
reducer soln (REST time) = soln {resting = time}
reducer soln (PH newPH) = soln {ph = mergePH (ph soln) (Just newPH), resting=0}
reducer soln (ADDITION newSoln _) = 
                          let newSalts = sort $ mergeSalts (salts soln) (salts newSoln);
                              previousAg = Ingredients.SilverNitrate.amount (agnox soln)
                              nextAg = Ingredients.SilverNitrate.amount (agnox newSoln)
                              newAgnox = SILVERNITRATE {Ingredients.SilverNitrate.amount = previousAg + nextAg}
                              newHalides = mergeHalides (agH soln) (agH newSoln)
                              newPh = mergePH (ph soln) (ph newSoln)
                          in SOLUTION {
                            salts = newSalts,
                            agnox = leftoverAGNO newAgnox newSalts,
                            agH = reactAGX newHalides newAgnox newSalts,
                            ph = newPh,
                            other = other soln ++ other newSoln,
                            water = water soln + water newSoln,
                            temp = Nothing,
                            resting = 0.0
                          } 


reactionDifference :: SilverNitrate -> Salt -> Double -- moles nitrate leftover
reactionDifference (SILVERNITRATE nitrate) salt = nitrateMoles - saltMoles
                                                where nitrateMW = molecularWeight (SILVERNITRATE nitrate)
                                                      saltMW = molecularWeight salt
                                                      nitrateMoles = nitrate / nitrateMW
                                                      saltMoles = Ingredients.Salt.amount salt / saltMW

leftoverAGNO :: SilverNitrate -> [Salt] -> SilverNitrate -- react all nitrate
leftoverAGNO (SILVERNITRATE 0) _ = SILVERNITRATE {Ingredients.SilverNitrate.amount=0}
leftoverAGNO s [] = s
leftoverAGNO nitrate (x:xs) = leftoverAGNO SILVERNITRATE { Ingredients.SilverNitrate.amount = max (reactionDifference nitrate x) 0.0 } xs

reactAGX :: [SilverHalide] -> SilverNitrate -> [Salt] -> [SilverHalide]
reactAGX sh sn (x:xs) = case x of 
                          (KI sq) -> reactAGX (sh `mergeHalides` [AgI {Ingredients.SilverHalide.moles = molesAGX}] ) (SILVERNITRATE {Ingredients.SilverNitrate.amount = leftoverGramsAGN}) xs
                          (KBr sq) -> reactAGX (sh `mergeHalides` [AgBr {Ingredients.SilverHalide.moles = molesAGX}] ) (SILVERNITRATE {Ingredients.SilverNitrate.amount = leftoverGramsAGN}) xs
                          (NaCl sq) -> reactAGX (sh `mergeHalides` [AgCl {Ingredients.SilverHalide.moles = molesAGX}] ) (SILVERNITRATE {Ingredients.SilverNitrate.amount = leftoverGramsAGN}) xs
  where mwSilverNitrate = molecularWeight sn;
        molesAGN = Ingredients.SilverNitrate.amount sn / mwSilverNitrate;
        molesX = Ingredients.Salt.amount x / molecularWeight x;
        molesAGX = min molesAGN molesX;
        leftoverGramsAGN = mwSilverNitrate * molesAGN - min molesAGN molesX
reactAGX sh sn [] = sh