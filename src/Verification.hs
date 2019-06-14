{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE DeriveAnyClass      #-}
module Verification (
  decodeToVerification
) where

import Data.SBV
import Data.Generics
import Control.Monad.Writer
import Data.Aeson.Types                     ( Parser )
import Data.Aeson                           ( FromJSON, ToJSON, eitherDecode )
import qualified Data.ByteString.Lazy as B
--
import Physics
import Ingredients.Salt
import qualified Step
import qualified State                        as E
import qualified Solution                     as S
import qualified Addition                     as A
import Ingredients.SilverNitrate             as N

decodeToVerification :: B.ByteString -> Either String (Writer String [E.State])
decodeToVerification arg = do
        raw <- inputs
        -- Right $ uncurry (toAnalysis nextState) raw
        Right $ do
          let final = map last $ uncurry E.makeStates raw
          tell $ show final
          return final
        where inputs = eitherDecode arg :: Either String (S.Solution, [Step.Step])

-- | A new data-type that we expect to use in an uninterpreted fashion
-- in the backend SMT solver. Note the custom @deriving@ clause, which
-- takes care of most of the boilerplate. The () field is needed so
-- SBV will not translate it to an enumerated data-type
newtype Emulsion = Emulsion () deriving (Eq, Ord, Data, Read, Show, SymVal, HasKind)

-- | Declare an uninterpreted function that works over Q's
f :: SBV Emulsion-> SBV Emulsion
f = uninterpret "f"

-- | A satisfiable example, stating that there is an element of the domain
-- 'Emulsion' such that 'f' returns a different element. Note that this is valid only
-- when the domain 'Emulsion' has at least two elements. We have:
--
-- >>> t1
-- Satisfiable. Model:
--   x = Q!val!0 :: Q
-- <BLANKLINE>
--   f :: Q -> Q
--   f _ = Q!val!1
t1 :: IO SatResult
t1 = sat $ do
        x <- free "x"
        return $ f x ./= x

-- | This is a variant on the first example, except we also add an axiom
-- for the sort, stating that the domain 'Q' has only one element. In this case
-- the problem naturally becomes unsat. We have:
--
-- >>> t2
-- Unsatisfiable
t2 :: IO SatResult
t2 = sat $ do x <- free "x"
              addAxiom "Emulsion" ["(assert (forall ((x Emulsion) (y Emulsion)) (= x y)))"]
              return $ f x ./= x