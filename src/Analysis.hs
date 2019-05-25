module Analysis (
  decodeToRecipe
, decoderTwo
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

decodeToRecipe :: B.ByteString -> Either String (Writer String S.Solution)
decodeToRecipe arg = do
        raw <- inputs
        Right $ uncurry (followRecipe nextStep) raw
        where inputs = eitherDecode arg :: Either String (S.Solution, [Step.Step])

decoderTwo :: B.ByteString -> Either String (Writer String E.State)
decoderTwo arg = do
    raw <- inputs
    Right $ do
        tell $ show $ E.stateAnalysis (fst raw) (snd raw)
        return $ E.stateAnalysis (fst raw) (snd raw)
    where inputs = eitherDecode arg :: Either String (S.Solution, [Step.Step])

-- decoderTwo :: B.ByteString -> Either String (Writer String Solution)
-- decoderTwo arg = do
--     raw <- inputs
--     let out = stateAnalysis (fst raw) (snd raw)
--     Right $ do
--           let final = analysis out
--           tell $ fst final
--           return $ snd final
--     where inputs = eitherDecode arg :: Either String (Solution, [Step])

-- asdf

type Mix = S.Solution -> Step.Step -> Writer String S.Solution

-- Output the emulsion making procedure into the writer monad
followRecipe :: Foldable f => Mix -> S.Solution -> f Step.Step -> Writer String S.Solution
followRecipe f b c = do
  tell $ "START WITH:\n" ++ S.prettySolution b
  foldM f b c

-- Function which advances the solution given the next step
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
  tell $ unlines $ map A.prettyAddition nextSolns
  return $ Step.moveStep soln a

