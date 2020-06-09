module MockData
  where

import Data.Time.Clock.POSIX
import qualified Data.Vector as V

import Types (Vertex(..))

import TestUtils (buildRateMatrix)

[ gdax_btc, gdax_usd, kraken_btc, kraken_usd, kraken_stc, 
  bittrex_btc, bittrex_usd, bittrex_stc] = 
    fmap (uncurry Vertex) [ ("GDAX", "BTC")
                          , ("GDAX", "USD")
                          , ("KRAKEN", "BTC")
                          , ("KRAKEN", "USD")
                          , ("KRAKEN", "STC")
                          , ("BITTREX", "BTC")
                          , ("BITTREX", "USD")
                          , ("BITTREX", "STC") ] 

[ d2017_11_01t09_42_23, d2017_11_01t09_43_23, d2017_11_01t09_42_24 ]  = 
  fmap posixSecondsToUTCTime [1509529343, 1509529403, 1509529344]

kraken_btc_usd = ((kraken_btc, kraken_usd), (1000.0, d2017_11_01t09_42_23))
kraken_usd_btc = ((kraken_usd, kraken_btc), (0.0009, d2017_11_01t09_42_23))

gdax_btc_usd = ((gdax_btc, gdax_usd), (1001.0, d2017_11_01t09_43_23))
gdax_usd_btc = ((gdax_usd, gdax_btc), (0.0008, d2017_11_01t09_43_23))
