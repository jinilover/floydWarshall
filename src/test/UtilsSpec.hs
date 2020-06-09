module UtilsSpec
  (specs) where

import Test.Hspec

import qualified Data.Set as S
import qualified Data.Vector as V

import Utils (setToVector)

import MockData ( gdax_btc
                , gdax_usd
                , kraken_btc
                , kraken_usd
                , gdax_usd
                , gdax_usd )

setToVectorSpec :: Spec
setToVectorSpec =
  describe "setToVectorSpec" $ do
    it "converts an empty Set" $
      setToVector S.empty `shouldBe` V.empty
    it "converts a Vertex Set to (Map Vertex Int, Vector Vectex) pair" $
      setToVector s `shouldBe` V.fromList [gdax_btc, gdax_usd, kraken_btc, kraken_usd]
  where
    s = S.fromList [kraken_btc, gdax_btc, kraken_usd, gdax_usd]

specs :: [Spec]
specs = [setToVectorSpec]
