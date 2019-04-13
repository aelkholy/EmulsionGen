module Main(main) where

import Data.List
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import qualified Test.Tasty.SmallCheck as SC
--
import Ingredients.Salt
import Ingredients.SilverNitrate
import Ingredients.SilverHalide
import qualified SilverGelatin       (silverReaction, coreHalideReaction)
import Ingredients.Basics           (Time, Temperature, Rate, Chemical(..))

main :: IO ()
main = defaultMain suite

arith :: Integer -> Integer -> Property
arith x y = (x > 0) && (y > 0) ==> (x+y)^2 > x^2 + y^2

negation :: Integer -> Bool
negation x = abs (x^2) >= x

suite :: TestTree
suite = testGroup "Test Suite" [
    -- testGroup "Units"
    --   [ testCase "Equality" $ True @=? True
    --   , testCase "Assertion" $ assert $ length [1,2,3] == 3
    --   ],

    testGroup "QuickCheck tests"
      [ testProperty "Salt order is preserved" saltOrderPreserved
      , testProperty "Silver >= 0" silverGoesToZero
      , testProperty "Silver can be > 0" silverIsPreserved
      , testProperty "halides can be > 0" halideReaction
      ]

    -- testGroup "SmallCheck tests"
    --   [ SC.testProperty "Negation" negation
    --   ]
  ]

saltOrderPreserved :: [Salt] -> [Salt] -> Bool
saltOrderPreserved x y = merged == sort merged where merged = mergeSalts (sort x) (sort y)

silverGoesToZero :: SilverNitrate -> [Salt] -> Bool
silverGoesToZero a b = Ingredients.SilverNitrate.amount ( SilverGelatin.silverReaction a b) >= 0

silverIsPreserved :: SilverNitrate -> Bool
silverIsPreserved b = if Ingredients.SilverNitrate.amount b > 0 then Ingredients.SilverNitrate.amount ( SilverGelatin.silverReaction b [KBr 10]) > 0 else True

halideReaction :: SilverNitrate -> Salt -> Bool
halideReaction _ _ = True
halideReaction sn@(SILVERNITRATE amt) s = case s of 
  x@(KI sq) -> if amt > 0 && sq > 0 then grams (head asdf) > 0 else True where asdf = SilverGelatin.coreHalideReaction sn [x]
  x@(KBr sq) -> if amt > 0 && sq > 0 then grams (head asdf) > 0 else True where asdf = SilverGelatin.coreHalideReaction sn [x]
  x@(NaCl sq) -> if amt > 0 && sq > 0 then grams (head asdf) > 0 else True where asdf = SilverGelatin.coreHalideReaction sn [x]

-- halideReactions :: SilverNitrate -> [Salt] -> Bool
-- halideReactions sn s = 

instance Arbitrary Salt where
  arbitrary = oneof [arbitraryIodide,arbitraryBromide,arbitraryChloride]
      where 
        arbitraryIodide = do
          p <- arbitrary
          return $ KI $ abs p
        arbitraryBromide = do
          p <- arbitrary
          return $ KBr $ abs p
        arbitraryChloride = do
          p <- arbitrary
          return $ NaCl $ abs p

instance Arbitrary SilverNitrate where
  arbitrary = do
    p <- arbitrary
    return $ SILVERNITRATE $ abs p