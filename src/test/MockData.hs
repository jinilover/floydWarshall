module MockData
  where

import Data.Time (UTCTime)
import Data.Time.Clock.POSIX

import qualified Data.Set as S
import qualified Data.Vector as V

import Hedgehog

import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Types (Vertex(..), Matrix, RateEntry(..))
import Utils (isolatedEntry)

sampleVertexTuples :: [(Text, Text)]
sampleVertexTuples = 
  [ ("GDAX", "BTC")
  , ("GDAX", "USD")
  , ("GDAX", "STC")
  , ("GDAX", "ETH")
  , ("GDAX", "ADA")
  , ("KRAKEN", "BTC")
  , ("KRAKEN", "USD")
  , ("KRAKEN", "STC")
  , ("KRAKEN", "ETH")
  , ("KRAKEN", "ADA")
  , ("BITTREX", "BTC")
  , ("BITTREX", "USD")
  , ("BITTREX", "STC")
  , ("BITTREX", "ETH")
  , ("BITTREX", "ADA") ] 

sampleVertices :: [Vertex]
sampleVertices = sampleVertexTuples <&> uncurry Vertex

[ gdax_btc, gdax_usd, gdax_stc, gdax_eth, gdax_ada, 
  kraken_btc, kraken_usd, kraken_stc, kraken_eth, kraken_ada, 
  bittrex_btc, bittrex_usd, bittrex_stc, bittrex_eth, bittrex_ada] = 
    sampleVertices

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

genUniqueVertices :: Gen (V.Vector Vertex)
genUniqueVertices = 
  let maxLength = length sampleVertices `div` 2 + 1 
  in  Gen.set (Range.linear 0 maxLength) genVertex <&> (V.fromList . S.toList)

genRateMatrix :: Gen (Matrix RateEntry)
genRateMatrix = do
  vertices <- genUniqueVertices
  let verticesSize = length vertices
  emptyRow <- Gen.bool
  return (
    if emptyRow 
      then 
        V.replicate verticesSize V.empty -- test empty matrix handlling
      else 
        vertices <&> \_start ->
          vertices <&> \dest -> 
            let entry = isolatedEntry _start
            in  if _start == dest then entry else entry {_bestRate = 1.0, _path = [dest]}
    )
