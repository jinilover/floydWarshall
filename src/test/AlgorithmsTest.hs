module AlgorithmsTest
  (test_Algorithms)
  where

import qualified Data.Map as M
import qualified Data.Vector as V

import Hedgehog
import Test.Tasty
import Test.Tasty.Hedgehog

import Algorithms (buildMatrix, floydWarshall, optimum)
import Types (RateEntry(..), Vertex, Matrix, AlgoError(..))
import Utils (isolatedEntry)

import MockData ( gdax_btc
                , gdax_usd
                , bittrex_btc
                , bittrex_stc
                , bittrex_usd
                , kraken_btc
                , kraken_stc
                , kraken_usd
                , kraken_btc_usd
                , kraken_usd_btc
                , gdax_btc_usd
                , gdax_usd_btc
                , genVertex
                , genRateMatrix )
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
    , testProperty "handle if it's reachable or not between src and dest" optimum_reachability
    , testProperty "the matrix may have no row or each row is empty" optimum_matrixMaybeEmpty ]
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

optimum' :: Vertex -> Vertex -> Matrix RateEntry -> Either AlgoError RateEntry
optimum' = optimum

optimum_srcOrDestNotExist :: Property
optimum_srcOrDestNotExist = 
  -- index in matrix first column: kraken_btc = 0, kraken_usd = 1, gdax_usd = 2, gdax_btc = 3
  let v = V.fromList [kraken_btc, kraken_usd, gdax_usd, gdax_btc]
      exchRates = M.fromList [kraken_btc_usd, kraken_usd_btc, gdax_btc_usd, gdax_usd_btc] <&> fst
      matrix = floydWarshall $ buildMatrix exchRates v
      findEntry src dest = optimum' src dest matrix
      expected = Left $ AlgoOptimumError "(KRAKEN, STC) is not entered before"
  in  property do
        (findEntry kraken_stc kraken_usd === expected) *>
          (findEntry kraken_btc kraken_stc === expected)

optimum_reachability :: Property
optimum_reachability = 
  -- index in matrix first column: gdax_btc = 0, gdax_usd = 1, kraken_btc = 2, kraken_usd = 3
  let v = V.fromList [gdax_btc, gdax_usd, kraken_btc, kraken_usd]
      exchRates = M.fromList [kraken_btc_usd, kraken_usd_btc, gdax_btc_usd, gdax_usd_btc] <&> fst
      -- built the optimised 4*4 matrix
      matrix = floydWarshall (buildMatrix exchRates v)
      lastRow = V.last matrix
      lastRow' = V.cons (isolatedEntry kraken_usd) (V.tail lastRow)
      -- by purpose set matrix[3][0] to be isolated to test if `optimum` check the reachability
      matrix' = V.take 3 matrix V.++ V.singleton lastRow'
      findEntry src dest = optimum' src dest matrix'
  in  property do
        -- matrix[3][0] is (0, []), so no exchange
        (findEntry kraken_usd gdax_btc === Left (AlgoOptimumError "There is no exchange between (KRAKEN, USD) and (GDAX, BTC)")) *>
          -- matrix[2][1] is (1001, [0,1])
          (findEntry kraken_btc gdax_usd === Right (RateEntry 1001.0 kraken_btc [gdax_btc, gdax_usd])) *>
          -- matrix[1][0] is (0.0009, [3,2,0])
          (findEntry gdax_usd gdax_btc === Right (RateEntry 0.0009 gdax_usd [kraken_usd, kraken_btc, gdax_btc]))

optimum_matrixMaybeEmpty :: Property
optimum_matrixMaybeEmpty = property do
  src <- forAll genVertex
  dest <- forAll genVertex
  matrix <- forAll genRateMatrix
  let result = optimum' src dest matrix
  checkResult result src dest matrix
  where
    checkResult result src dest matrix
      | V.null matrix = 
          result === Left (AlgoOptimumError $ tshow src <> " is not entered before")
      | any null matrix = 
          result === Left (AlgoOptimumError "The matrix is empty")
      | not (contains src matrix) = 
          result === Left (AlgoOptimumError $ tshow src <> " is not entered before")
      | not (contains dest matrix) = 
          result === Left (AlgoOptimumError $ tshow dest <> " is not entered before")
      | src == dest = 
          result === Left (AlgoOptimumError $ "There is no exchange between " <> tshow src <> " and " <> tshow dest)
      | otherwise = 
          result === Right (RateEntry 1.0 src [dest])
    contains vertex matrix = flip any matrix \row -> 
                              flip any row \RateEntry{..} -> _start == vertex
