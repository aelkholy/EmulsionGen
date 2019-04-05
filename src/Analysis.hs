module Analysis ( 
  analyze
  ) where

import SilverGelatin

analyze :: Either String Emulsion -> Either String String
analyze e = Right "asdf"

-- getHalidePercent :: Emulsion -> (Double, Double, Double)
-- getHalidePercent e =

maybeMerge :: Maybe [a] -> Maybe [a] -> Maybe [a]
maybeMerge maybeA maybeB =
  do a <- maybeA   -- if A is something, get it; otherwise, pass through Nothing
     b <- maybeB   -- if B is something, get it; otherwise, pass through Nothing
     Just (merge a b)  -- both inputs succeeded!  do the operation, and return the result

accumulate :: Solution -> Solution -> Solution
accumulate s1 s2 = Solution maybeMerge s1.salts s2.salts, Nothing, Nothing, Nothing, Nothing, Nothing


-- accumulateSolutions :: Emulsion -> Solution
-- accumulateSolutions (EMULSION is t) = is t

-- data Analysis

-- prettyAnalysis :: Analysis -> String