module ProcessRequestsTest 
  ( test_ProcessRequests )
  where

import Control.Monad.RWS.CPS
import Data.List (nub)

import qualified Data.Map as M

import Hedgehog
import Test.Tasty
import Test.Tasty.Hedgehog

import Algorithms (floydWarshall)
import ProcessRequests (serveReq, findBestRate, updateRates)
import Types (RateEntry(..)
            , AppState(..)
            , DisplayMessage(..)
            , AppError(..)
            , ParseError(..)
            , AlgoError(..)
            , ExchRateTimes)
import Utils (updateMap, blankState)

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

serveReq' :: RWST Text DisplayMessage AppState (Either AppError) ()
serveReq' = serveReq

serveReq_bothInvalid :: Property
serveReq_bothInvalid = 
  let err = [ "Failed reading: parseTimeM: no parse of \"2017-11-0109:42:23+00:00\""
            , "Invalid request to update rates, probably a request for best rate"
            , "letter: Failed reading: satisfy" ]
      states = [inSyncExRates2, outSyncExRates2] 
      expected = Right $ states <&> ((), , mempty {_err = err})
      result = flip traverse states $ 
                runRWST serveReq' "2017-11-0109:42:23+00:00 KRAKEN BTC USD 1000.0 0.0009"
  in  property do result === expected

serveReq_updateRates :: Property
serveReq_updateRates = 
  let res = [ "(KRAKEN, BTC) -- 1000.0 2017-11-01 09:42:23 UTC --> (KRAKEN, USD)"
            , "(KRAKEN, USD) -- 9.0e-4 2017-11-01 09:42:23 UTC --> (KRAKEN, BTC)" ]
      expected = Right ((), outSyncExRates1, mempty {_res = res})
      result = runRWST serveReq' "2017-11-01T09:42:23+00:00 KRAKEN BTC USD 1000.0 0.0009" blankState
  in  property do result === expected

serveReq_findBestRate :: Property
serveReq_findBestRate = 
  let err = [ "Failed reading: parseTimeM: no parse of \"KRAKEN\""
            , "Invalid request to update rates, probably a request for best rate" ]
      res = [ "BEST_RATES_BEGIN KRAKEN BTC KRAKEN USD 1001.0"
            , "(KRAKEN, BTC)"
            , "(GDAX, BTC)"
            , "(GDAX, USD)"
            , "(KRAKEN, USD)"
            , "BEST_RATES_END" ]
      expected = Right ((), inSyncExRates2, mempty {_err = err, _res = res})
      result = runRWST serveReq' "KRAKEN BTC KRAKEN USD" outSyncExRates2
  in  property do result === expected

-- just for info, `updateRates` can be 
-- ReaderT Text (StateT AppState (ExceptT ParseError Identity)) ()
-- but `RWST` is simpler
updateRates' :: RWST Text () AppState (Either ParseError) ()
updateRates' = updateRates

updateRates_addRateToEmptyState :: Property
updateRates_addRateToEmptyState = property do
  runRWST updateRates' "2017-11-01T09:42:23+00:00 KRAKEN BTC USD 1000.0 0.0009" blankState
    === Right ((), outSyncExRates1, ())

updateRates_turnStateOutSync :: Property
updateRates_turnStateOutSync = 
  let origStates = [inSyncExRates1, outSyncExRates1] -- no matter it's originally `InSync` or `OutSync`
      result = flip traverse origStates $ 
                runRWST updateRates' "2017-11-01T09:43:23+00:00 GDAX BTC USD 1001.0 0.0008"
      s = OutSync $ updateMap [gdax_btc_usd, gdax_usd_btc] exRates1 
      expected = Right $ replicate 2 ((), s, ())
  in  property do result === expected

updateRates_onlyUpdateByNewerTs :: Property
updateRates_onlyUpdateByNewerTs = 
  let -- should update because timestamp `2017-11-01T09:42:24+00:00` is newer than
      -- those of `_exchRates` in the orig state and the update is case insensitive
      inputStrings =  [ "2017-11-01T09:42:24+00:00 KRAKEN USD BTC 0.00089 1001.1"
                      , "2017-11-01T09:42:24+00:00 kraken usd btc 0.00089 1001.1"]
      result = flip traverse inputStrings \r -> runRWST updateRates' r outSyncExRates2
      s = OutSync . updateMap [ ((kraken_btc, kraken_usd), (1001.1, d2017_11_01t09_42_24))
                              , ((kraken_usd, kraken_btc), (0.00089, d2017_11_01t09_42_24)) ]
                              $ exRates2
      expected = Right $ replicate 2 ((), s, ())
  in  property do result === expected

updateRates_notNewerTs :: Property
updateRates_notNewerTs = 
  let inputStrings =  [ "2017-11-01T09:42:20+00:00 KRAKEN USD BTC 0.00089 1001.1"
                      , "2017-11-01T09:42:23+00:00 KRAKEN USD BTC 0.00089 1001.1" ] 
      origStates = [inSyncExRates1, outSyncExRates1]
      result = sequence . nub $ runRWST updateRates' <$> inputStrings <*> origStates
      expected = Right (origStates <&> ((), , ()))
  in  property do result === expected

findBestRate' :: RWST Text () AppState (Either AppError) RateEntry
findBestRate' = findBestRate

findBestRate_srcNotExists :: Property
findBestRate_srcNotExists = 
  let result = runRWST findBestRate' "KRAKEN STC GDAX USD" outSyncExRates2
      expected = Left . AppAlgoError . AlgoOptimumError $ "(KRAKEN, STC) is not entered before"
  in  property do result === expected

findBestRate_destNotExists :: Property
findBestRate_destNotExists = 
  let result = runRWST findBestRate' "KRAKEN USD GDAX STC" outSyncExRates2
      expected = Left . AppAlgoError . AlgoOptimumError $ "(GDAX, STC) is not entered before"
  in  property do result === expected

findBestRate_sameUiSameResult :: Property
findBestRate_sameUiSameResult = 
  let origStates = [inSyncExRates2, outSyncExRates2] 
      result = flip traverse origStates $ runRWST findBestRate' "KRAKEN BTC KRAKEN USD"
      _bestRate = 1001.0
      _start = kraken_btc
      _path = [gdax_btc, gdax_usd, kraken_usd]
      expected = Right $ replicate 2 (RateEntry{..}, inSyncExRates2, ()) 
  in  property do result === expected

-- sample data for convenience 
exRates1 :: ExchRateTimes
exRates1 = M.fromList [kraken_btc_usd, kraken_usd_btc]

exRates2 :: ExchRateTimes
exRates2 = updateMap [gdax_btc_usd, gdax_usd_btc] exRates1

[inSyncExRates1, inSyncExRates2, outSyncExRates1, outSyncExRates2] = 
  [inSync, OutSync] <*> [exRates1, exRates2]

inSync :: ExchRateTimes -> AppState
inSync exRates = 
  let timeRemoved = M.map fst exRates
  in  InSync exRates $ floydWarshall timeRemoved