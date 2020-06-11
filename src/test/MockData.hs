module MockData
  where

import Data.Time (UTCTime)
import Data.Time.Clock.POSIX

import qualified Data.Set as S

import Hedgehog

import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Types (Vertex(..))

sampleVertexTuples :: [(Text, Text)]
sampleVertexTuples = 
  [ ("GDAX", "BTC")
  , ("GDAX", "USD")
  , ("KRAKEN", "BTC")
  , ("KRAKEN", "USD")
  , ("KRAKEN", "STC")
  , ("BITTREX", "BTC")
  , ("BITTREX", "USD")
  , ("BITTREX", "STC") ] 

sampleVertices :: [Vertex]
sampleVertices = sampleVertexTuples <&> uncurry Vertex

[ gdax_btc, gdax_usd, kraken_btc, kraken_usd, kraken_stc, 
  bittrex_btc, bittrex_usd, bittrex_stc] = sampleVertices

[ d2017_11_01t09_42_23, d2017_11_01t09_43_23, d2017_11_01t09_42_24 ]  = 
  fmap posixSecondsToUTCTime [1509529343, 1509529403, 1509529344]

kraken_btc_usd :: ((Vertex, Vertex), (Double, UTCTime))
kraken_btc_usd = ((kraken_btc, kraken_usd), (1000.0, d2017_11_01t09_42_23))

kraken_usd_btc :: ((Vertex, Vertex), (Double, UTCTime))
kraken_usd_btc = ((kraken_usd, kraken_btc), (0.0009, d2017_11_01t09_42_23))

gdax_btc_usd :: ((Vertex, Vertex), (Double, UTCTime))
gdax_btc_usd = ((gdax_btc, gdax_usd), (1001.0, d2017_11_01t09_43_23))

gdax_usd_btc :: ((Vertex, Vertex), (Double, UTCTime))
gdax_usd_btc = ((gdax_usd, gdax_btc), (0.0008, d2017_11_01t09_43_23))

genVertex :: Gen Vertex
genVertex = Gen.element sampleVertices

genVertexSet :: Gen (S.Set Vertex)
genVertexSet = Gen.set (Range.linear 1 (length sampleVertices)) genVertex