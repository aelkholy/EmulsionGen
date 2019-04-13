module Main(main) where

import Data.List
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import qualified Test.Tasty.SmallCheck    as SC
--
import qualified Ingredients.Salt         as IS
import qualified Ingredients.SilverHalide as ISH
import Ingredients.SilverNitrate
import SilverGelatin                     (silverReaction, halideReaction, saltReaction)
import Ingredients.Basics                (Time, Temperature, Rate, Chemical(..))


main :: IO ()
main = defaultMain suite

suite :: TestTree
suite = testGroup "Test Suite" [
    -- testGroup "Units"
    --   [ testCase "Equality" $ True @=? True
    --   , testCase "Assertion" $ assert $ length [1,2,3] == 3
    --   ],

    -- testGroup "SmallCheck tests"
    --   [ SC.testProperty "Negation" negation
    --   ],

    -- testGroup "Chemical reaction tests"
    --   [ SC.testProperty "Negation" negation
    --   ],

    -- testGroup "silver reaction tests"
    --   [ 
    --   ],

    testGroup "salt / silver reaction tests"
      [ testProperty "silver after salt reaction is smaller" silversRemovedBySalt
      , testProperty "Salts after silver reaction is smaller" saltsRemovedBySilver
      , testProperty "Halgen quantities preserved" sameHalideOutputAmounts
      , testProperty "Not producing duplicate halogens" onlyThreeHalides
      ],

    testGroup "Salt tests"
      [ testProperty "Salt reduced to 3 or less in merge" saltReduction
      , testProperty "Salt quantities preservied in merge" saltPreservation
      ],

      testGroup "Halide tests"
      [ testProperty "Halides reduced to 3 or less in merge" halideReduction
      , testProperty "Halide quantities preservied in merge" halidePreservation
      ]
  ]

-- halideReaction :: SilverNitrate -> [Salt] -> [SilverHalide]
onlyThreeHalides :: SilverNitrate -> [IS.Salt] -> Bool
onlyThreeHalides a b = length output <= 3
  where output = halideReaction a (IS.mergeSalts [] b)

-- halideReaction :: SilverNitrate -> [Salt] -> [SilverHalide]
sameHalideOutputAmounts :: SilverNitrate -> [IS.Salt] -> Bool
sameHalideOutputAmounts a b = length output == length b
  where output = halideReaction a b

-- silverReaction :: SilverNitrate -> [Salt] -> SilverNitrate
silversRemovedBySalt :: SilverNitrate -> [IS.Salt] -> Bool
silversRemovedBySalt a b = moles outputSilver <=  moles a
  where outputSilver = silverReaction a b

-- saltReaction :: SilverNitrate -> [Salt] -> [Salt]
saltsRemovedBySilver :: SilverNitrate -> [IS.Salt] -> Bool
saltsRemovedBySilver a b = sum (map moles outputSalt) <= sum ( map moles b )
  where outputSalt = saltReaction a b

saltReduction :: [IS.Salt] -> [IS.Salt] -> Bool
saltReduction a b = i <= 1 && br <= 1 && na <= 1
  where output = IS.mergeSalts a b
        i = length [ x | x@(IS.KI _) <- output ]
        br = length [ x | x@(IS.KBr _) <- output ]
        na = length [ x | x@(IS.NaCl _) <- output ]

saltPreservation :: [IS.Salt] -> [IS.Salt] -> Bool
saltPreservation a b = abs (sumA + sumB) - foldl (+) 0.0 (map grams (IS.mergeSalts a b)) <= 0.00001
  where sumA = foldl (+) 0.0 (map grams a)
        sumB = foldl (+) 0.0 (map grams b)

halideReduction :: [ISH.SilverHalide] -> [ISH.SilverHalide] -> Bool
halideReduction a b = i <= 1 && br <= 1 && na <= 1
  where output = ISH.mergeHalides a b
        i = length [ x | x@(ISH.AgI _) <- output ]
        br = length [ x | x@(ISH.AgBr _) <- output ]
        na = length [ x | x@(ISH.AgCl _) <- output ]

halidePreservation :: [ISH.SilverHalide] -> [ISH.SilverHalide] -> Bool
halidePreservation a b = abs (sumA + sumB) - foldl (+) 0.0 (map grams (ISH.mergeHalides a b)) <= 0.00001
  where sumA = foldl (+) 0.0 (map grams a)
        sumB = foldl (+) 0.0 (map grams b)

instance Arbitrary IS.Salt where
  arbitrary = oneof [arbitraryIodide,arbitraryBromide,arbitraryChloride]
      where 
        arbitraryIodide = do
          p <- arbitrary
          return $ IS.KI $ abs p
        arbitraryBromide = do
          p <- arbitrary
          return $ IS.KBr $ abs p
        arbitraryChloride = do
          p <- arbitrary
          return $ IS.NaCl $ abs p

instance Arbitrary ISH.SilverHalide where
  arbitrary = oneof [arbitraryIodide,arbitraryBromide,arbitraryChloride]
      where 
        arbitraryIodide = do
          p <- arbitrary
          return $ ISH.AgI $ abs p
        arbitraryBromide = do
          p <- arbitrary
          return $ ISH.AgBr $ abs p
        arbitraryChloride = do
          p <- arbitrary
          return $ ISH.AgCl $ abs p

instance Arbitrary SilverNitrate where
  arbitrary = do
    p <- arbitrary
    return $ SILVERNITRATE $ abs p