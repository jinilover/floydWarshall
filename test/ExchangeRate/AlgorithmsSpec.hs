module ExchangeRate.AlgorithmsSpec
  (specs)
  where

import Test.Hspec

import Protolude
import Data.Time

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Vector as V

import ExchangeRate.DataTypes
import ExchangeRate.Algorithms

import ExchangeRate.MockData

buildMatrixSpec :: Spec
buildMatrixSpec =
  describe "buildMatrixSpec" $ do
    it "can handle no vertex" $
      buildMatrix M.empty V.empty `shouldBe` V.empty
    it "build a matrix expected to be inputMatrix1 4*4" $ do
      time <- getCurrentTime
      buildMatrix (createRates1 time) v1 `shouldBe` inputMatrix1
    it "build a matrix expected to be inputMatrix2 7*7" $ do
      time <- getCurrentTime
      buildMatrix (createRates2 time) v2 `shouldBe` inputMatrix2
  where v1 = V.fromList [kraken_btc, kraken_usd, gdax_usd, gdax_btc]
        createRates1 time = M.fromList
          [ ((gdax_btc, gdax_usd), (1001, time))
          , ((kraken_btc, kraken_usd), (1000, time))
          , ((gdax_usd, gdax_btc), (0.0008, time))
          , ((kraken_usd, kraken_btc), (0.0009, time))]
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

floydWarshallSpec :: Spec
floydWarshallSpec =
  describe "floydWarshallSpec" $ do
    it "floydWarshall can handle empty matrix" $
      floydWarshall 0 V.empty `shouldBe` V.empty
    it "floydWarshall on inputMatrix1 4*4" $
      floydWarshall 0 inputMatrix1 `shouldBe` outputMatrix1
    it "floydWarshall on inputMatrix2 7*7" $
      (path <$>) <$> floydWarshall 0 inputMatrix2 `shouldBe` outputPathsMatrix2

optimumPathSpec :: Spec
optimumPathSpec =
  describe "optimumPathSpec" $ do
    it "Source not exists in the graph" $
      optimumPath (kraken_stc, kraken_usd) vertexSet outputMatrix1 `shouldBe`
        ["Both source and destination must have been entered before"]
    it "Destination not exists in the graph" $
      optimumPath (kraken_btc, kraken_stc) vertexSet outputMatrix1 `shouldBe`
        ["Both source and destination must have been entered before"]
    it "Source cannot reach destination in the graph" $
      optimumPath (kraken_usd, gdax_btc) vertexSet outputMatrix1' `shouldBe`
        ["Not reachable"]
    it "Reachable from kraken_btc to gdax_usd" $
      optimumPath (kraken_btc, gdax_usd) vertexSet outputMatrix1' `shouldBe`
        [ "BEST_RATES_BEGIN KRAKEN BTC GDAX USD 1001.0"
        , "KRAKEN, BTC"
        , "GDAX, BTC"
        , "GDAX, USD"
        , "BEST_RATES_END"]
    it "Reachable from gdax_usd to gdax_btc" $
      optimumPath (gdax_usd, gdax_btc) vertexSet outputMatrix1' `shouldBe`
        [ "BEST_RATES_BEGIN GDAX USD GDAX BTC 9.0e-4"
        , "GDAX, USD"
        , "KRAKEN, USD"
        , "KRAKEN, BTC"
        , "GDAX, BTC"
        , "BEST_RATES_END"]
  where vertexSet = S.fromList [kraken_btc, kraken_usd, gdax_usd, gdax_btc]

specs :: [Spec]
specs = [buildMatrixSpec, floydWarshallSpec, optimumPathSpec]
