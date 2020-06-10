{-# LANGUAGE TupleSections #-}
module ProcessRequestsTest 
  ( test_ProcessRequests )
  where

import Control.Lens
import Control.Monad.RWS.CPS
import Data.List (nub)

import qualified Data.Map as M
import qualified Data.Set as S

import Hedgehog
import Test.Tasty
import Test.Tasty.Hedgehog

import Algorithms (buildMatrix, floydWarshall)
import ProcessRequests (serveReq, findBestRate, updateRates)
import Types (UserInput(..)
            , exchRateTimes
            , vertices
            , RateEntry(..)
            , AppState(..)
            , DisplayMessage(..))
import Utils (updateMap, updateSet, blankState, setToVector)

import MockData ( gdax_btc
                , gdax_usd
                , gdax_btc_usd
                , gdax_usd_btc
                , kraken_btc
                , kraken_usd
                , kraken_btc_usd
                , kraken_usd_btc
                , d2017_11_01t09_42_24 )

test_ProcessRequests :: TestTree
test_ProcessRequests = testGroup "ProcessRequests"
  [ testGroup "serveReq"
    [ testGroup "invalid input"
      [ testProperty "invalid input for both updateRates and findBestRate" serveReq_bothInvalid ]
    , testGroup "valid input"
      [ testProperty 
          "valid `updateRates` request, so not proceed to `findBestRate` request" 
          serveReq_updateRates
      , testProperty "invalid `updateRates`, proceed to `findBestRate`" serveReq_findBestRate ]
    ]
  , testGroup "updateRates"
    [ testProperty "update empty state with added rates" updateRates_addRateToEmptyState
    , testProperty "update always makes the state `OutSync`" updateRates_turnStateOutSync
    , testProperty "only input having newer timestamp make the update" updateRates_onlyUpdateByNewerTs
    , testProperty "not update if input doesn't have newer timestamp" updateRates_notNewerTs ]
  , testGroup "findBestRate"
    [ testGroup "invalid input"
      [ testProperty "source vertex not exists" findBestRate_srcNotExists
      , testProperty "destination vertex not exists" findBestRate_destNotExists
      , testProperty 
          "same `UserInput` always produces the same result no matter the state is `InSync` or `OutSync`" 
          findBestRate_sameUiSameResult ]
    ]
  ]

serveReq_bothInvalid :: Property
serveReq_bothInvalid = 
  let err = [ "System unexpecting: \" \""
            , "General error: Invalid timestamp: 2017-11-0109:42:23+00:00"
            , "Invalid request to update rates, probably a request for best rate"
            , "System unexpecting: \"2\""
            , "System unexpecting: \"2\""
            , "Expecting: space"
            , "Expecting: letter" ]
      states = [inSyncUi2, outSyncUi2] 
      expected = Right $ states <&> ((), , mempty {_err = err})
      result = flip traverse states $ 
                runRWST serveReq "2017-11-0109:42:23+00:00 KRAKEN BTC USD 1000.0 0.0009"
  in  property do result === expected

serveReq_updateRates :: Property
serveReq_updateRates = 
  let res = [ "(KRAKEN, BTC) -- 1000.0 2017-11-01 09:42:23 UTC --> (KRAKEN, USD)"
            , "(KRAKEN, USD) -- 9.0e-4 2017-11-01 09:42:23 UTC --> (KRAKEN, BTC)" ]
      expected = Right ((), outSyncUi1, mempty {_res = res})
      result = runRWST serveReq "2017-11-01T09:42:23+00:00 KRAKEN BTC USD 1000.0 0.0009" blankState
  in  property do result === expected

serveReq_findBestRate :: Property
serveReq_findBestRate = 
  let err = [ "System unexpecting: \" \""
            , "General error: Invalid timestamp: KRAKEN"
            , "Invalid request to update rates, probably a request for best rate" ]
      res = [ "BEST_RATES_BEGIN KRAKEN BTC KRAKEN USD 1001.0"
            , "(KRAKEN, BTC)"
            , "(GDAX, BTC)"
            , "(GDAX, USD)"
            , "(KRAKEN, USD)"
            , "BEST_RATES_END" ]
      expected = Right ((), inSyncUi2, mempty {_err = err, _res = res})
      result = runRWST serveReq "KRAKEN BTC KRAKEN USD" outSyncUi2
  in  property do result === expected

updateRates_addRateToEmptyState :: Property
updateRates_addRateToEmptyState = property do
  runRWST updateRates "2017-11-01T09:42:23+00:00 KRAKEN BTC USD 1000.0 0.0009" blankState
    === Right ((), outSyncUi1, ())

updateRates_turnStateOutSync :: Property
updateRates_turnStateOutSync = 
  let origStates = [inSyncUi1, outSyncUi1] -- no matter it's originally `InSync` or `OutSync`
      result = flip traverse origStates $ 
                runRWST updateRates "2017-11-01T09:43:23+00:00 GDAX BTC USD 1001.0 0.0008"
      s = OutSync $ ui1 & exchRateTimes %~ updateMap [gdax_btc_usd, gdax_usd_btc]
                        & vertices %~ updateSet [gdax_btc, gdax_usd]
      expected = Right $ replicate 2 ((), s, ())
  in  property do result === expected

updateRates_onlyUpdateByNewerTs :: Property
updateRates_onlyUpdateByNewerTs = 
  let -- should update because timestamp `2017-11-01T09:42:24+00:00` is newer than
      -- those of `_exchRates` in the orig state and the update is case insensitive
      inputStrings =  [ "2017-11-01T09:42:24+00:00 KRAKEN USD BTC 0.00089 1001.1"
                      , "2017-11-01T09:42:24+00:00 kraken usd btc 0.00089 1001.1"]
      result = flip traverse inputStrings \r -> runRWST updateRates r outSyncUi2
      s = OutSync $ ui2 & exchRateTimes %~ updateMap [ ((kraken_btc, kraken_usd), (1001.1, d2017_11_01t09_42_24))
                                                 , ((kraken_usd, kraken_btc), (0.00089, d2017_11_01t09_42_24)) ]
      expected = Right $ replicate 2 ((), s, ())
  in  property do result === expected

updateRates_notNewerTs :: Property
updateRates_notNewerTs = 
  let inputStrings =  [ "2017-11-01T09:42:20+00:00 KRAKEN USD BTC 0.00089 1001.1"
                      , "2017-11-01T09:42:23+00:00 KRAKEN USD BTC 0.00089 1001.1" ] 
      origStates = [inSyncUi1, outSyncUi1]
      result = sequence . nub $ runRWST updateRates <$> inputStrings <*> origStates
      expected = Right (origStates <&> ((), , ()))
  in  property do result === expected

findBestRate_srcNotExists :: Property
findBestRate_srcNotExists = 
  let result :: Either [Text] (RateEntry, AppState, ())
      result = runRWST findBestRate "KRAKEN STC GDAX USD" outSyncUi2
  in  property do result === Left ["(KRAKEN, STC) is not entered before"]

findBestRate_destNotExists :: Property
findBestRate_destNotExists = 
  let result :: Either [Text] (RateEntry, AppState, ())
      result = runRWST findBestRate "KRAKEN USD GDAX STC" outSyncUi2
  in  property do result === Left ["(GDAX, STC) is not entered before"]

findBestRate_sameUiSameResult :: Property
findBestRate_sameUiSameResult = 
  let origStates = [inSyncUi2, outSyncUi2] 
      result = flip traverse origStates $
                runRWST findBestRate "KRAKEN BTC KRAKEN USD"
      _bestRate = 1001.0
      _start = kraken_btc
      _path = [gdax_btc, gdax_usd, kraken_usd]
      expected = Right $ replicate 2 (RateEntry{..}, inSyncUi2, ()) 
  in  property do result === expected

-- sample data for convenience 
ui1 :: UserInput
ui1 = let _exchRateTimes = M.fromList [kraken_btc_usd, kraken_usd_btc]
          _vertices = S.fromList [kraken_btc, kraken_usd]
      in  UserInput{..}

ui2 :: UserInput
ui2 = ui1 & exchRateTimes %~ updateMap [gdax_btc_usd, gdax_usd_btc]
          & vertices %~ updateSet [gdax_btc, gdax_usd]

[inSyncUi1, inSyncUi2, outSyncUi1, outSyncUi2] = 
  [inSync, OutSync] <*> [ui1, ui2]

inSync :: UserInput -> AppState
inSync ui@UserInput{..} = 
  let vector = setToVector _vertices
      exchRates = M.map fst _exchRateTimes
      matrix = floydWarshall $ buildMatrix exchRates vector
  in  InSync ui matrix