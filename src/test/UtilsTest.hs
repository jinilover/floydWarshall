module UtilsTest 
  (test_Utils)
  where

import Data.List (sort)
import qualified Data.Set as S
import qualified Data.Vector as V

import Hedgehog
import Test.Tasty
import Test.Tasty.Hedgehog

import Utils (setToVector)

import MockData (genVertexSet)

test_Utils :: TestTree
test_Utils = testGroup "Utils"
  [ testGroup "setToVector" 
    [ testProperty "converts an empty Set" setToVector_emptySet 
    , testProperty "converts a non-empty Set" setToVector_nonEmptySet ]
  ]

setToVector_emptySet :: Property
setToVector_emptySet = property do
  setToVector S.empty === V.empty

setToVector_nonEmptySet :: Property
setToVector_nonEmptySet = property do
  s <- forAll genVertexSet
  let v = setToVector s
  ((S.fromList . V.toList) v === s) *> 
    ((V.fromList . sort . V.toList) v === v)