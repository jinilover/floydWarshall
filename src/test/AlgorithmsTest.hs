module AlgorithmsTest
  (test_Algorithms)
  where

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Vector as V

import Hedgehog
import Test.Tasty
import Test.Tasty.Hedgehog

import Algorithms (buildMatrix, floydWarshall, optimum)
import Types (RateEntry(..))
import Utils (setToVector)

import MockData ( gdax_btc
                , gdax_usd
                , bittrex_btc
                , bittrex_stc
                , bittrex_usd
                , kraken_btc
                , kraken_stc
                , kraken_usd )
import TestUtils (rateMatrixForTest, listsToMatrix)

test_Algorithms :: TestTree
test_Algorithms = testGroup "Algorithms"
  [ testGroup "buildMatrix"
    [ testProperty "build empty matrix for empty vertex" buildMatrix_emptyMatrix 
    , testProperty "build a 4*4 matrix" buildMatrix_4x4Matrix
    , testProperty "build a 7*7 matrix" buildMatrix_7x7Matrix ]
  , testGroup "floydWarshall"
    [ testProperty "handle empty matrix" floydWarshall_emptyMatrix
    , testProperty "handle 4*4 matrix" floydWarshall_4x4Matrix
    , testProperty "handle 7*7 matrix" floydWarshall_7x7Matrix ]
  , testGroup "optimum"
    [ testProperty "src or dest not exists in the graph" optimum_srcOrDestNotExist 
    , testProperty "handle if it's reachable or not between src and dest" optimum_reachability]
  ]

buildMatrix_emptyMatrix :: Property
buildMatrix_emptyMatrix = property do
  buildMatrix M.empty V.empty === V.empty

buildMatrix_4x4Matrix :: Property
buildMatrix_4x4Matrix = 
  let v = V.fromList [kraken_btc, kraken_usd, gdax_usd, gdax_btc]
      rates = M.fromList
        [ ((gdax_btc, gdax_usd), 1001)
        , ((kraken_btc, kraken_usd), 1000)
        , ((gdax_usd, gdax_btc), 0.0008)
        , ((kraken_usd, kraken_btc), 0.0009) ]
      result = buildMatrix rates v
      expected = rateMatrixForTest v
        [ [(0, []),       (1000, [1]), (0, []),     (1, [3])]
        , [(0.0009, [0]), (0, []),     (1, [2]),    (0, [])]
        , [(0, []),       (1, [1]),    (0, []),     (0.0008, [3])]
        , [(1, [0]),      (0, []),     (1001, [2]), (0, [])]
        ]
  in  property do result === expected

buildMatrix_7x7Matrix :: Property
buildMatrix_7x7Matrix = 
  let v = V.fromList [kraken_btc, kraken_usd, gdax_usd, gdax_btc, bittrex_btc, bittrex_usd, bittrex_stc]
      rates = M.fromList
        [ ((bittrex_usd, bittrex_btc), 0.001)
        , ((kraken_usd, kraken_btc), 0.0009)
        , ((gdax_usd, gdax_btc), 0.0008)
        , ((bittrex_usd, bittrex_stc), 0.00085)
        , ((bittrex_stc, bittrex_btc), 1.007)
        , ((gdax_btc, gdax_usd), 1001)
        , ((bittrex_btc, bittrex_usd), 998)
        , ((bittrex_btc, bittrex_stc), 0.9)
        , ((kraken_btc, kraken_usd), 1000)
        , ((bittrex_stc, bittrex_usd), 1000)
        ]
      result = buildMatrix rates v
      expected = rateMatrixForTest v
        [ [(0, []),       (1000, [1]),  (0, []),      (1, [3]),       (1, [4]),     (0, []),      (0, [])]
        , [(0.0009, [0]), (0, []),      (1, [2]),     (0, []),        (0, []),      (1, [5]),     (0, [])]
        , [(0, []),       (1, [1]),     (0, []),      (0.0008, [3]),  (0, []),      (1, [5]),     (0, [])]
        , [(1, [0]),      (0, []),      (1001, [2]),  (0, []),        (1, [4]),     (0, []),      (0, [])]
        , [(1, [0]),      (0, []),      (0, []),      (1, [3]),       (0, []),      (998, [5]),   (0.9, [6])]
        , [(0, []),       (1, [1]),     (1, [2]),     (0, []),        (0.001, [4]), (0, []),      (0.00085, [6])]
        , [(0, []),       (0, []),      (0, []),      (0, []),        (1.007, [4]), (1000, [5]),  (0, [])]
        ]
  in  property do result === expected

floydWarshall_emptyMatrix :: Property
floydWarshall_emptyMatrix = property do
  floydWarshall V.empty === V.empty

floydWarshall_4x4Matrix :: Property
floydWarshall_4x4Matrix = 
  let v = V.fromList [kraken_btc, kraken_usd, gdax_usd, gdax_btc]
      origMatrix = rateMatrixForTest v 
        [ [(0, []),       (1000, [1]), (0, []),     (1, [3])]
        , [(0.0009, [0]), (0, []),     (1, [2]),    (0, [])]
        , [(0, []),       (1, [1]),    (0, []),     (0.0008, [3])]
        , [(1, [0]),      (0, []),     (1001, [2]), (0, [])]
        ]
      result = floydWarshall origMatrix
      expected = rateMatrixForTest v
        [ [(0, []),         (1001, [3,2,1]), (1001, [3,2]), (1, [3])]
        , [(0.0009, [0]),   (0, []),         (1, [2]),      (0.0009, [0,3])]
        , [(0.0009, [1,0]), (1, [1]),        (0, []),       (0.0009, [1,0,3])]
        , [(1, [0]),        (1001, [2,1]),   (1001, [2]),   (0, [])]
        ]
  in  property do result === expected

floydWarshall_7x7Matrix :: Property
floydWarshall_7x7Matrix = 
  let v = V.fromList [kraken_btc, kraken_usd, gdax_usd, gdax_btc, bittrex_btc, bittrex_usd, bittrex_stc]
      origMatrix = rateMatrixForTest v
        [ [(0, []),       (1000, [1]),  (0, []),      (1, [3]),       (1, [4]),     (0, []),      (0, [])]
        , [(0.0009, [0]), (0, []),      (1, [2]),     (0, []),        (0, []),      (1, [5]),     (0, [])]
        , [(0, []),       (1, [1]),     (0, []),      (0.0008, [3]),  (0, []),      (1, [5]),     (0, [])]
        , [(1, [0]),      (0, []),      (1001, [2]),  (0, []),        (1, [4]),     (0, []),      (0, [])]
        , [(1, [0]),      (0, []),      (0, []),      (1, [3]),       (0, []),      (998, [5]),   (0.9, [6])]
        , [(0, []),       (1, [1]),     (1, [2]),     (0, []),        (0.001, [4]), (0, []),      (0.00085, [6])]
        , [(0, []),       (0, []),      (0, []),      (0, []),        (1.007, [4]), (1000, [5]),  (0, [])]
        ]
      -- no need to check the rate as it's done in the previous test case
      -- only need to check the path
      result = floydWarshall origMatrix <&> (<&> _path)
      expected = listsToMatrix $ fmap (fmap (<&> (v V.!))) 
        [ [[], [3,2,5,4,3,2,1], [3,2,5,4,3,2], [3,2,5,4,3], [3,2,5,4], [3,2,5], [3,2,5,4,6]]
        , [[5,4,0], [], [5,4,3,2], [5,4,3], [5,4], [5], [5,4,6]]
        , [[5,4,0], [5,4,3,2,1], [], [5,4,3], [5,4], [5], [5,4,6]]
        , [[2,5,4,0], [2,5,4,3,2,1], [2,5,4,3,2], [], [2,5,4], [2,5], [2,5,4,6]]
        , [[3,2,5,4,0], [3,2,5,4,3,2,1], [3,2,5,4,3,2], [3,2,5,4,3], [], [3,2,5], [3,2,5,4,6]]
        , [[4,0], [4,3,2,1], [4,3,2], [4,3], [4], [], [4,6]]
        , [[4,3,2,5,4,0], [4,3,2,5,4,3,2,1], [4,3,2,5,4,3,2], [4,3,2,5,4,3], [4,3,2,5,4], [4,3,2,5], []]
        ]
    in  property do result === expected

optimum_srcOrDestNotExist :: Property
optimum_srcOrDestNotExist = 
  let v = setToVector $ S.fromList [kraken_btc, kraken_usd, gdax_usd, gdax_btc]
      matrix = rateMatrixForTest v
        [ [(0, []),         (1001, [3,2,1]), (1001, [3,2]), (1, [3])]
        , [(0.0009, [0]),   (0, []),         (1, [2]),      (0.0009, [0,3])]
        , [(0.0009, [1,0]), (1, [1]),        (0, []),       (0.0009, [1,0,3])]
        , [(1, [0]),        (1001, [2,1]),   (1001, [2]),   (0, [])]
        ]
      findEntry src dest = optimum src dest matrix
  in  property do
        (findEntry kraken_stc kraken_usd === Left "(KRAKEN, STC) is not entered before") *>
          (findEntry kraken_btc kraken_stc === Left "(KRAKEN, STC) is not entered before")

optimum_reachability :: Property
optimum_reachability = 
  let v = setToVector $ S.fromList [kraken_btc, kraken_usd, gdax_usd, gdax_btc]
      matrix = rateMatrixForTest v
        [ [(0, []),           (1001, [1]),    (1, [2]),        (1001, [1,3])]
        , [(0.0009, [3,2,0]), (0, []),        (0.0009, [3,2]), (1, [3])]
        , [(1, [0]),          (1001, [0,1]),  (0, []),         (1001, [0,1,3])]
        , [(0, []),           (1, [1]),       (0.0009, [2]),   (0, [])]
        ]
      findEntry src dest = optimum src dest matrix
  in  property do
        (findEntry kraken_usd gdax_btc === Left "There is no exchange between (KRAKEN, USD) and (GDAX, BTC)") *>
          (findEntry kraken_btc gdax_usd === Right (RateEntry 1001.0 kraken_btc [gdax_btc, gdax_usd])) *>
          (findEntry gdax_usd gdax_btc === Right (RateEntry 0.0009 gdax_usd [kraken_usd, kraken_btc, gdax_btc]))