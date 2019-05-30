{-# LANGUAGE ViewPatterns #-}
module Analysis (
    decodeToProcedure
  , decodeToAnalysis
  ) where

-- Home team
import Physics
import qualified Step
import qualified Emulsion                     as E
import qualified Solution                     as S
import qualified Addition                     as A
-- FROM STACKAGE
import Data.Aeson                           ( FromJSON, ToJSON, eitherDecode )
import Data.Aeson.Types                     ( Parser )
import Control.Monad
import Control.Monad.Writer
import System.IO  
import System.Environment
import qualified Data.ByteString.Lazy as B

-- Marshal input json functions

decodeToProcedure :: B.ByteString -> Either String (Writer String S.Solution)
decodeToProcedure arg = do
        raw <- inputs
        Right $ uncurry (toProcedure nextStep) raw
        where inputs = eitherDecode arg :: Either String (S.Solution, [Step.Step])

decodeToAnalysis :: B.ByteString -> Either String (Writer String [E.State])
decodeToAnalysis arg = do
        raw <- inputs
        let states = uncurry E.makeStates raw
        Right $ toAnalysis analyzeStates states
        -- Right $ do
        --   let final = map last $ uncurry E.makeStates raw
        --   tell $ show final
        --   return final
        where inputs = eitherDecode arg :: Either String (S.Solution, [Step.Step])

--

type Analyze = [E.State] -> [E.State] -> Writer String [E.State]

-- This will fold over the states to output the analysis of an emulsion
toAnalysis :: Analyze -> [[E.State]] -> Writer String [E.State]
toAnalysis a states = foldM a firstState (tail states)
  where firstState = head states

analyzeStates :: [E.State] -> [E.State] -> Writer String [E.State]
analyzeStates stateOne stateTwo = do
  tell $ analyzeState stateOne
  -- normalize state in process
  return stateTwo

analyzeState :: [E.State] -> String
analyzeState (reverse -> x@((E.NOTHING s a t):_)) = unlines [
    "Nothing" ++ show x
  ]
analyzeState a@(reverse -> (x:_)) = unlines [
    show a
  ]


type Procedure = S.Solution -> Step.Step -> Writer String S.Solution

-- This will fold over the solution and steps to output the procedure for making an emulsion
toProcedure :: Foldable f => Procedure -> S.Solution -> f Step.Step -> Writer String S.Solution
toProcedure f b c = do
  tell $ "START WITH:\n" ++ S.prettySolution b
  foldM f b c

-- This will move stepwise through the procedure and output the next step
nextStep :: S.Solution -> Step.Step -> Writer String S.Solution
nextStep soln (Step.TEMPERATURE newTemp) = do
  tell $ unlines [ unwords ["SET TEMPERATURE TO", prettyTemperature newTemp] ]
  return soln
nextStep soln (Step.REST minutes) = do
  tell $ unlines [ unwords ["REST FOR", show minutes, "MINUTES"] ]
  return soln
nextStep soln Step.WASH = do
  tell $ unlines ["WASH EMULSION"]
  return $ S.washSolution soln
nextStep soln a@(Step.ADDITION nextSolns) = do
  tell $ unlines $ "ADD" : map A.prettyAddition nextSolns
  return $ Step.moveStep soln a