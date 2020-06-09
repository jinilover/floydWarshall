module AlgorithmsSpec
  (specs)
  where

import Test.Hspec

import Data.Time

import qualified Data.Map as M
import qualified Data.Set as S
import Data.Vector as V

import Types
import Algorithms
import Utils (setToVector)

import MockData
import TestUtils (listsToMatrix, buildRateMatrix)

buildMatrixSpec :: Spec
buildMatrixSpec =
  describe "buildMatrixSpec" $ do
    it "can handle no vertex" $
      buildMatrix M.empty V.empty `shouldBe` V.empty
    it "build a 4*4 matrix" $ do
      time <- getCurrentTime
      buildMatrix (createRates1 time) v1 `shouldBe` expectedMatrix1
    it "build a 7*7 matrix" $ do
      time <- getCurrentTime
      buildMatrix (createRates2 time) v2 `shouldBe` expectedMatrix2
  where
    v1 = V.fromList [kraken_btc, kraken_usd, gdax_usd, gdax_btc]
    createRates1 time = M.fromList
      [ ((gdax_btc, gdax_usd), (1001, time))
      , ((kraken_btc, kraken_usd), (1000, time))
      , ((gdax_usd, gdax_btc), (0.0008, time))
      , ((kraken_usd, kraken_btc), (0.0009, time))]
    expectedMatrix1 = buildRateMatrix v1
      [ [(0, []),       (1000, [1]), (0, []),     (1, [3])]
      , [(0.0009, [0]), (0, []),     (1, [2]),    (0, [])]
      , [(0, []),       (1, [1]),    (0, []),     (0.0008, [3])]
      , [(1, [0]),      (0, []),     (1001, [2]), (0, [])]
      ]
    v2 = V.fromList [kraken_btc, kraken_usd, gdax_usd, gdax_btc, bittrex_btc, bittrex_usd, bittrex_stc]
    createRates2 time = M.fromList
      [ ((bittrex_usd, bittrex_btc), (0.001, time))
      , ((kraken_usd, kraken_btc), (0.0009, time))
      , ((gdax_usd, gdax_btc), (0.0008, time))
      , ((bittrex_usd, bittrex_stc), (0.00085, time))
      , ((bittrex_stc, bittrex_btc), (1.007, time))
      , ((gdax_btc, gdax_usd), (1001, time))
      , ((bittrex_btc, bittrex_usd), (998, time))
      , ((bittrex_btc, bittrex_stc), (0.9, time))
      , ((kraken_btc, kraken_usd), (1000, time))
      , ((bittrex_stc, bittrex_usd), (1000, time))
      ]
    expectedMatrix2 = buildRateMatrix v2
      [ [(0, []),       (1000, [1]),  (0, []),      (1, [3]),       (1, [4]),     (0, []),      (0, [])]
      , [(0.0009, [0]), (0, []),      (1, [2]),     (0, []),        (0, []),      (1, [5]),     (0, [])]
      , [(0, []),       (1, [1]),     (0, []),      (0.0008, [3]),  (0, []),      (1, [5]),     (0, [])]
      , [(1, [0]),      (0, []),      (1001, [2]),  (0, []),        (1, [4]),     (0, []),      (0, [])]
      , [(1, [0]),      (0, []),      (0, []),      (1, [3]),       (0, []),      (998, [5]),   (0.9, [6])]
      , [(0, []),       (1, [1]),     (1, [2]),     (0, []),        (0.001, [4]), (0, []),      (0.00085, [6])]
      , [(0, []),       (0, []),      (0, []),      (0, []),        (1.007, [4]), (1000, [5]),  (0, [])]
      ]

floydWarshallSpec :: Spec
floydWarshallSpec =
  describe "floydWarshallSpec" $ do
    it "floydWarshall can handle empty matrix" $
      floydWarshall V.empty `shouldBe` V.empty
    it "floydWarshall on 4*4 matrix" $
      floydWarshall matrix1 `shouldBe` expectedMatrix1
    it "floydWarshall on 7*7 matrix" $
      -- no need to check the rate as it's done in the previous test case
      -- only need to check the path
      (_path <$>) <$> floydWarshall matrix2 `shouldBe` expectedPaths
  where
    v1 = V.fromList [kraken_btc, kraken_usd, gdax_usd, gdax_btc]
    matrix1 = buildRateMatrix v1 
      [ [(0, []),       (1000, [1]), (0, []),     (1, [3])]
      , [(0.0009, [0]), (0, []),     (1, [2]),    (0, [])]
      , [(0, []),       (1, [1]),    (0, []),     (0.0008, [3])]
      , [(1, [0]),      (0, []),     (1001, [2]), (0, [])]
      ]
    expectedMatrix1 = buildRateMatrix v1
      [ [(0, []),         (1001, [3,2,1]), (1001, [3,2]), (1, [3])]
      , [(0.0009, [0]),   (0, []),         (1, [2]),      (0.0009, [0,3])]
      , [(0.0009, [1,0]), (1, [1]),        (0, []),       (0.0009, [1,0,3])]
      , [(1, [0]),        (1001, [2,1]),   (1001, [2]),   (0, [])]
      ]
    v2 = V.fromList [kraken_btc, kraken_usd, gdax_usd, gdax_btc, bittrex_btc, bittrex_usd, bittrex_stc]
    matrix2 = buildRateMatrix v2
      [ [(0, []),       (1000, [1]),  (0, []),      (1, [3]),       (1, [4]),     (0, []),      (0, [])]
      , [(0.0009, [0]), (0, []),      (1, [2]),     (0, []),        (0, []),      (1, [5]),     (0, [])]
      , [(0, []),       (1, [1]),     (0, []),      (0.0008, [3]),  (0, []),      (1, [5]),     (0, [])]
      , [(1, [0]),      (0, []),      (1001, [2]),  (0, []),        (1, [4]),     (0, []),      (0, [])]
      , [(1, [0]),      (0, []),      (0, []),      (1, [3]),       (0, []),      (998, [5]),   (0.9, [6])]
      , [(0, []),       (1, [1]),     (1, [2]),     (0, []),        (0.001, [4]), (0, []),      (0.00085, [6])]
      , [(0, []),       (0, []),      (0, []),      (0, []),        (1.007, [4]), (1000, [5]),  (0, [])]
      ]
    expectedPaths = listsToMatrix $ fmap (fmap (<&> (v2 !))) 
      [
        [[], [3,2,5,4,3,2,1], [3,2,5,4,3,2], [3,2,5,4,3], [3,2,5,4], [3,2,5], [3,2,5,4,6]]
      , [[5,4,0], [], [5,4,3,2], [5,4,3], [5,4], [5], [5,4,6]]
      , [[5,4,0], [5,4,3,2,1], [], [5,4,3], [5,4], [5], [5,4,6]]
      , [[2,5,4,0], [2,5,4,3,2,1], [2,5,4,3,2], [], [2,5,4], [2,5], [2,5,4,6]]
      , [[3,2,5,4,0], [3,2,5,4,3,2,1], [3,2,5,4,3,2], [3,2,5,4,3], [], [3,2,5], [3,2,5,4,6]]
      , [[4,0], [4,3,2,1], [4,3,2], [4,3], [4], [], [4,6]]
      , [[4,3,2,5,4,0], [4,3,2,5,4,3,2,1], [4,3,2,5,4,3,2], [4,3,2,5,4,3], [4,3,2,5,4], [4,3,2,5], []]
      ]

optimumSpec :: Spec
optimumSpec =
  describe "optimumSpec" $ do
    it "Source not exists in the graph" $
      optimum kraken_stc kraken_usd vertexVector matrix1 `shouldBe`
        Left "(KRAKEN, STC) is not entered before"
    it "Destination not exists in the graph" $
      optimum kraken_btc kraken_stc vertexVector matrix1 `shouldBe`
        Left "(KRAKEN, STC) is not entered before"
    it "Source cannot reach destination in the graph" $
      optimum kraken_usd gdax_btc vertexVector matrix2 `shouldBe`
        Left "There is no exchange between (KRAKEN, USD) and (GDAX, BTC)"
    it "Reachable from kraken_btc to gdax_usd" $
      optimum kraken_btc gdax_usd vertexVector matrix2 `shouldBe`
        Right (RateEntry 1001.0 kraken_btc [gdax_btc, gdax_usd])
    it "Reachable from gdax_usd to gdax_btc" $
      optimum gdax_usd gdax_btc vertexVector matrix2 `shouldBe`
        Right (RateEntry 0.0009 gdax_usd [kraken_usd, kraken_btc, gdax_btc])
  where
    vertices = [kraken_btc, kraken_usd, gdax_usd, gdax_btc]
    vertexVector = setToVector $ S.fromList vertices
    matrix1 = buildRateMatrix vertexVector
      [ [(0, []),         (1001, [3,2,1]), (1001, [3,2]), (1, [3])]
      , [(0.0009, [0]),   (0, []),         (1, [2]),      (0.0009, [0,3])]
      , [(0.0009, [1,0]), (1, [1]),        (0, []),       (0.0009, [1,0,3])]
      , [(1, [0]),        (1001, [2,1]),   (1001, [2]),   (0, [])]
      ]
    matrix2 = buildRateMatrix vertexVector
      [ [(0, []),           (1001, [1]),    (1, [2]),        (1001, [1,3])]
      , [(0.0009, [3,2,0]), (0, []),        (0.0009, [3,2]), (1, [3])]
      , [(1, [0]),          (1001, [0,1]),  (0, []),         (1001, [0,1,3])]
      , [(0, []),           (1, [1]),       (0.0009, [2]),   (0, [])]
      ]

specs :: [Spec]
specs = [buildMatrixSpec, floydWarshallSpec, optimumSpec]
