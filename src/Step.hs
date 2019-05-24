{-# LANGUAGE DeriveGeneric #-}
module Step (
    Step(..)
  , Mix
  , foldRecipe
  , followRecipe
  , mixer
  ) where

-- Hackage
import Data.Maybe
import GHC.Generics
import Control.Monad.Writer
import Control.Monad                 (foldM)
-- Homies
import Solution
import Physics
import Ingredients.Ingredient        (Chemical(..))
import qualified Addition             as A


-- Steps you can take

data Step = TEMPERATURE {newCelsius :: Temperature}
 | ADDITION {additions :: [A.Addition]}
 | REST {minutes :: Integer}
 | WASH deriving (Generic, Show)

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
    return $ washSolution soln
followRecipe soln a@(ADDITION nextSolns) = do
  tell $ unlines $ map A.prettyAddition nextSolns
  return $ mixer soln a

mixer :: Solution -> Step -> Solution
mixer soln (TEMPERATURE t) = soln
mixer soln (REST t) = soln
mixer soln WASH = soln
mixer soln (ADDITION sols) = foldl addSolutions x xs
    where x = A.solution $ head sols
          xs = map A.solution $ tail sols