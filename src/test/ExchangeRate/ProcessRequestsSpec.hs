{-# LANGUAGE TupleSections #-}
module ExchangeRate.ProcessRequestsSpec
  (specs)
  where

import Test.Hspec
import Test.HUnit.Lang

import Data.String (String)
import Control.Monad.RWS.CPS
import Data.List
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Vector as V

import ExchangeRate.DataTypes
import ExchangeRate.Utils
import ExchangeRate.ProcessRequests
import ExchangeRate.Constants
import ExchangeRate.MockData
import ExchangeRate.TestUtils

combineRWSTSpec :: Spec
combineRWSTSpec =
  describe "combineRWSTSpec" $ do
    it "run valid updateRates, it shouldn't proceed to findBestRate" $
      let w = [ "(KRAKEN, BTC) -- 1000.0 2017-11-01 09:42:23 UTC --> (KRAKEN, USD)"
              , "(KRAKEN, USD) -- 9.0e-4 2017-11-01 09:42:23 UTC --> (KRAKEN, BTC)" ]
          s = OutSync ui1
          expected = (True, s, w) in
      runRWST combineRWST "2017-11-01T09:42:23+00:00 KRAKEN BTC USD 1000.0 0.0009"
        (OutSync emptyUserInput) >>= (`shouldBe` expected)
    it "invalid updateRates, it should proceed to findBestRate" $
      let w = [ "System unexpecting: \" \""
              , "General error: Invalid timestamp: KRAKEN"
              , "Invalid request to update rates, probably a request for best rate"
              , "BEST_RATES_BEGIN KRAKEN BTC KRAKEN USD 1001.0"
              , "KRAKEN, BTC"
              , "GDAX, BTC"
              , "GDAX, USD"
              , "KRAKEN, USD"
              , "BEST_RATES_END" ]
          expected = (True, inSyncUi2, w) in
      runRWST combineRWST "KRAKEN BTC KRAKEN USD"
        outSyncUi2 >>= (`shouldBe` expected)
    it "both updateRates and findBestRate are invalid, it should display all eror message" $
      let w = [ "System unexpecting: \" \""
              , "General error: Invalid timestamp: 2017-11-0109:42:23+00:00"
              , "Invalid request to update rates, probably a request for best rate","System unexpecting: \"2\""
              , "System unexpecting: \"2\""
              , "Expecting: space"
              , "Expecting: letter" ]
          appStates = [inSyncUi2, outSyncUi2] in
      traverse
        (runRWST combineRWST "2017-11-0109:42:23+00:00 KRAKEN BTC USD 1000.0 0.0009")
        appStates >>= (`shouldBe` (False, , w) <$> appStates)

updateRatesSpec :: Spec
updateRatesSpec =
  describe "updateRatesSpec" $ do
    it "orig AppState has empty UserInput, success update should return OutSync with added rates" $
      let a = [ "(KRAKEN, BTC) -- 1000.0 2017-11-01 09:42:23 UTC --> (KRAKEN, USD)"
              , "(KRAKEN, USD) -- 9.0e-4 2017-11-01 09:42:23 UTC --> (KRAKEN, BTC)" ]
          s = OutSync ui1
          expected = Right (a, s, ()) in
      runRWST updateRates "2017-11-01T09:42:23+00:00 KRAKEN BTC USD 1000.0 0.0009"
        (OutSync emptyUserInput) `shouldBe` expected
    it "no matter orig AppState is either InSync or OutSync, success update should return OutSync of added rates" $
      let a = [ "(GDAX, BTC) -- 1001.0 2017-11-01 09:43:23 UTC --> (GDAX, USD)" -- entry due to added rate
              , "(GDAX, USD) -- 8.0e-4 2017-11-01 09:43:23 UTC --> (GDAX, BTC)" -- entry due to added rate
              , "(KRAKEN, BTC) -- 1000.0 2017-11-01 09:42:23 UTC --> (KRAKEN, USD)"
              , "(KRAKEN, USD) -- 9.0e-4 2017-11-01 09:42:23 UTC --> (KRAKEN, BTC)" ]
          s = OutSync $ ui1 { exchRates = updateMap (exchRates ui1) [gdax_btc_usd, gdax_usd_btc]
                                      , vertices = updateSet (vertices ui1) [gdax_btc, gdax_usd] }
          expected = Right (a, s, ())
          appStates = [InSync ui1 emptyMatrix, OutSync ui1] in
      runRWST updateRates "2017-11-01T09:43:23+00:00 GDAX BTC USD 1001.0 0.0008" <$>
        appStates `shouldBe` replicate 2 expected
    it "update the rate by the newer timestamp no matter the exchange currency cases" $
      let a = [ "(GDAX, BTC) -- 1001.0 2017-11-01 09:43:23 UTC --> (GDAX, USD)"
              , "(GDAX, USD) -- 8.0e-4 2017-11-01 09:43:23 UTC --> (GDAX, BTC)"
              , "(KRAKEN, BTC) -- 1001.1 2017-11-01 09:42:24 UTC --> (KRAKEN, USD)" -- entry due to newer ts
              , "(KRAKEN, USD) -- 8.9e-4 2017-11-01 09:42:24 UTC --> (KRAKEN, BTC)" ] -- entry due to new ts
          s = OutSync $ ui2 { exchRates = updateMap (exchRates ui2)
                              [((kraken_btc, kraken_usd), (1001.1, d2017_11_01t09_42_24)), ((kraken_usd, kraken_btc), (0.00089, d2017_11_01t09_42_24))] }
          expected = Right (a, s, ())
          strings = [ "2017-11-01T09:42:24+00:00 KRAKEN USD BTC 0.00089 1001.1"
                    , "2017-11-01T09:42:24+00:00 kraken usd btc 0.00089 1001.1"] in
      (\r -> runRWST updateRates r $ OutSync ui2) <$> strings `shouldBe` replicate 2 expected
    it "no matter orig AppState is either InSync or OutSync, AppState remain the same as input ts are not newer" $
      let a = [ "(KRAKEN, BTC) -- 1000.0 2017-11-01 09:42:23 UTC --> (KRAKEN, USD)"
              , "(KRAKEN, USD) -- 9.0e-4 2017-11-01 09:42:23 UTC --> (KRAKEN, BTC)" ]
          appStates = [InSync ui1 emptyMatrix, OutSync ui1]
          strings = [ "2017-11-01T09:42:20+00:00 KRAKEN USD BTC 0.00089 1001.1"
                    , "2017-11-01T09:42:23+00:00 KRAKEN USD BTC 0.00089 1001.1" ] in
      nub (runRWST updateRates <$> strings <*> appStates)
        `shouldBe` Right . (a, , ()) <$> appStates

findBestRateSpec :: Spec
findBestRateSpec =
  describe "findBestRateSpec" $ do
    it "failed due to source vertex not exists" $
      runRWST findBestRate "KRAKEN STC GDAX USD" outSyncUi2
        `shouldBe` Left ["(KRAKEN, STC) is not entered before"]
    it "failed due to dest vertex not exists" $
      runRWST findBestRate "KRAKEN USD GDAX STC" outSyncUi2
        `shouldBe` Left ["(GDAX, STC) is not entered before"]
    it "no matter orig AppState is InSync or OutSync, it will show the same path and return InSync" $
      let a = [ "BEST_RATES_BEGIN KRAKEN BTC KRAKEN USD 1001.0"
              , "KRAKEN, BTC"
              , "GDAX, BTC"
              , "GDAX, USD"
              , "KRAKEN, USD"
              , "BEST_RATES_END" ]
          expected = Right (a, inSyncUi2, ()) in
      runRWST findBestRate "KRAKEN BTC KRAKEN USD" <$> [inSyncUi2, outSyncUi2] `shouldBe` replicate 2 expected

outSyncUi2 :: AppState
outSyncUi2 = OutSync ui2

inSyncUi2 :: AppState
inSyncUi2 = InSync ui2 $ tuplesToMatrix bestMatrix
  where
    bestMatrix = [ [(0.0, []), (1001.0, [1]), (1.0, [2]), (1001.0, [1,3])]
                 , [(0.0009, [3,2,0]), (0.0, []), (0.0009, [3,2]), (1.0, [3])]
                 , [(1.0, [0]), (1001.0, [0,1]), (0.0, []), (1001.0, [0,1,3])]
                 , [(0.0009, [2,0]), (1.0, [1]), (0.0009, [2]), (0.0, [])] ]

ui1 :: UserInput
ui1 = UserInput (M.fromList [kraken_btc_usd, kraken_usd_btc]) (S.fromList [kraken_btc, kraken_usd])

ui2 :: UserInput
ui2 = ui1 { exchRates = updateMap (exchRates ui1) [gdax_btc_usd, gdax_usd_btc]
          , vertices = updateSet (vertices ui1) [gdax_btc, gdax_usd] }

emptyMatrix :: Matrix
emptyMatrix = V.empty -- updateRates doesn't care the matrix value

-- expectErrorMsg :: Either [String] a -> String -> Expectation
-- expectErrorMsg (Left msgs) s = msgs `shouldSatisfy` elem s
-- expectErrorMsg _ s = assertFailure ("Expected error msg: " ++ s)

specs :: [Spec]
specs = [combineRWSTSpec, findBestRateSpec, updateRatesSpec]