module ExchangeRate.UtilsSpec
  (specs) where

import Test.Hspec

import Protolude
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Vector as V

import ExchangeRate.DataTypes
import ExchangeRate.Utils

import ExchangeRate.MockData

setToMapVectorSpec :: Spec
setToMapVectorSpec =
  describe "setToMapVectorSpec" $ do
    it "converts an empty Set" $
      setToMapVector S.empty `shouldBe` (M.empty, V.empty)
    it "converts a Vertex Set to (Map Vertex Int, Vector Vectex) pair" $
      setToMapVector s `shouldSatisfy` (\(m, v) ->
        m == M.fromList [(kraken_btc, 2), (gdax_btc, 0), (kraken_usd, 3), (gdax_usd, 1)] &&
        m == M.fromList [(gdax_btc, 0), (gdax_usd, 1), (kraken_btc, 2), (kraken_usd, 3)] &&
        v == V.fromList [gdax_btc, gdax_usd, kraken_btc, kraken_usd]
      )
  where s = S.fromList [kraken_btc, gdax_btc, kraken_usd, gdax_usd]

specs :: [Spec]
specs = [setToMapVectorSpec]
