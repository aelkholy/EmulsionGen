module Analysis ( 
  analyze
  ) where

import SilverGelatin (Solution(..), Emulsion(..))
import Ingredients.Basics (Quantity(..), Chemical(..), Unit(..))

data SilverHalide = SilverIodide {quantity :: Quantity} | SilverBromide {quantity :: Quantity} | SilverChloride {quantity :: Quantity}

-- instance Chemical SilverHalide where
--   molecularWeight (SilverIodide _) = 60
--   molecularWeight (SilverBromide _) = 50
--   molecularWeight (SilverChloride _) = 40
  
-- solutionAccum :: Emulsion -> Solution
-- finalHalides emulsion = foldl (emulsion.initialSolution) (emulsion.transitions)

-- finalHalides :: Solution -> [SilverHalide]
-- finalHalides s = do
--   silver <- s.ag
--   iSalts <- s.salts
--   let salts = sort iSalts

analyze :: Either String SilverGelatin.Emulsion -> Either String String
analyze x = Right "Hi"