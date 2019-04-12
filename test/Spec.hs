module Main(main) where

import Data.List
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import qualified Test.Tasty.SmallCheck as SC
--
import Ingredients.Salt

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
      ]

    -- testGroup "SmallCheck tests"
    --   [ SC.testProperty "Negation" negation
    --   ]
  ]

saltOrderPreserved :: [Salt] -> [Salt] -> Bool
saltOrderPreserved x y = merged == sort merged where merged = mergeSalts (sort x) (sort y)

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