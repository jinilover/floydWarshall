module ExchangeRate.MockData
  where

import Protolude
import Data.Time.Clock.POSIX
import qualified Data.Vector as V

import ExchangeRate.DataTypes

import ExchangeRate.TestUtils

gdax_btc = Vertex "GDAX" "BTC"
gdax_usd = Vertex "GDAX" "USD"
kraken_btc = Vertex "KRAKEN" "BTC"
kraken_usd = Vertex "KRAKEN" "USD"
kraken_stc = Vertex "KRAKEN" "STC"
bittrex_btc = Vertex "BITTREX" "BTC"
bittrex_usd = Vertex "BITTREX" "USD"
bittrex_stc = Vertex "BITTREX" "STC"

d2017_11_01t09_42_23 = posixSecondsToUTCTime 1509529343
d2017_11_01t09_43_23 = posixSecondsToUTCTime 1509529403
d2017_11_01t09_42_24 = posixSecondsToUTCTime 1509529344

kraken_btc_usd = ((kraken_btc, kraken_usd), (1000.0, d2017_11_01t09_42_23))
kraken_usd_btc = ((kraken_usd, kraken_btc), (0.0009, d2017_11_01t09_42_23))

gdax_btc_usd = ((gdax_btc, gdax_usd), (1001.0, d2017_11_01t09_43_23))
gdax_usd_btc = ((gdax_usd, gdax_btc), (0.0008, d2017_11_01t09_43_23))

inputMatrix1 :: Matrix
inputMatrix1 = tuplesToMatrix
  [ [(0, []),       (1000, [1]), (0, []),     (1, [3])]
  , [(0.0009, [0]), (0, []),     (1, [2]),    (0, [])]
  , [(0, []),       (1, [1]),    (0, []),     (0.0008, [3])]
  , [(1, [0]),      (0, []),     (1001, [2]), (0, [])]
  ]

outputMatrix1 :: Matrix
outputMatrix1 = tuplesToMatrix
  [ [(0, []),         (1001, [3,2,1]), (1001, [3,2]), (1, [3])]
  , [(0.0009, [0]),   (0, []),         (1, [2]),      (0.0009, [0,3])]
  , [(0.0009, [1,0]), (1, [1]),        (0, []),       (0.0009, [1,0,3])]
  , [(1, [0]),        (1001, [2,1]),   (1001, [2]),   (0, [])]
  ]

outputMatrix1' :: Matrix
outputMatrix1' = tuplesToMatrix
  [ [(0, []),           (1001, [1]),    (1, [2]),        (1001, [1,3])]
  , [(0.0009, [3,2,0]), (0, []),        (0.0009, [3,2]), (1, [3])]
  , [(1, [0]),          (1001, [0,1]),  (0, []),         (1001, [0,1,3])]
  , [(0, []),           (1, [1]),       (0.0009, [2]),   (0, [])]
  ]

inputMatrix2 :: Matrix
inputMatrix2 = tuplesToMatrix
  [ [(0, []),       (1000, [1]),  (0, []),      (1, [3]),       (1, [4]),     (0, []),      (0, [])]
  , [(0.0009, [0]), (0, []),      (1, [2]),     (0, []),        (0, []),      (1, [5]),     (0, [])]
  , [(0, []),       (1, [1]),     (0, []),      (0.0008, [3]),  (0, []),      (1, [5]),     (0, [])]
  , [(1, [0]),      (0, []),      (1001, [2]),  (0, []),        (1, [4]),     (0, []),      (0, [])]
  , [(1, [0]),      (0, []),      (0, []),      (1, [3]),       (0, []),      (998, [5]),   (0.9, [6])]
  , [(0, []),       (1, [1]),     (1, [2]),     (0, []),        (0.001, [4]), (0, []),      (0.00085, [6])]
  , [(0, []),       (0, []),      (0, []),      (0, []),        (1.007, [4]), (1000, [5]),  (0, [])]
  ]

outputPathsMatrix2 :: V.Vector (V.Vector [Int])
outputPathsMatrix2 = V.fromList ( V.fromList <$>
  [
    [[], [3,2,5,4,3,2,1], [3,2,5,4,3,2], [3,2,5,4,3], [3,2,5,4], [3,2,5], [3,2,5,4,6]]
  , [[5,4,0], [], [5,4,3,2], [5,4,3], [5,4], [5], [5,4,6]]
  , [[5,4,0], [5,4,3,2,1], [], [5,4,3], [5,4], [5], [5,4,6]]
  , [[2,5,4,0], [2,5,4,3,2,1], [2,5,4,3,2], [], [2,5,4], [2,5], [2,5,4,6]]
  , [[3,2,5,4,0], [3,2,5,4,3,2,1], [3,2,5,4,3,2], [3,2,5,4,3], [], [3,2,5], [3,2,5,4,6]]
  , [[4,0], [4,3,2,1], [4,3,2], [4,3], [4], [], [4,6]]
  , [[4,3,2,5,4,0], [4,3,2,5,4,3,2,1], [4,3,2,5,4,3,2], [4,3,2,5,4,3], [4,3,2,5,4], [4,3,2,5], []]
  ] )
