{-# LANGUAGE TupleSections #-}
module ProcessRequestsSpec
  (specs)
  where

import Test.Hspec

import Control.Monad.RWS.CPS
import Data.List
import Data.String (String)
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Vector as V

import Types (AppState(..), DisplayMessage(..), RateEntry(..), Matrix, UserInput(..))
import Utils (updateMap, updateSet, emptyUserInput)
import ProcessRequests (serveReq, findBestRate, updateRates)

import MockData ( kraken_btc
                , kraken_usd
                , kraken_btc_usd
                , kraken_usd_btc
                , gdax_btc
                , gdax_usd
                , gdax_btc_usd
                , gdax_usd_btc
                , d2017_11_01t09_42_24 )
import TestUtils (buildRateMatrix)

serveReqSpec :: Spec
serveReqSpec = 
  describe "serveReqSpec" $ do
    let rwst = serveReq :: RWST String DisplayMessage AppState (Either [String]) ()
    it "run valid updateRates, it shouldn't proceed to findBestRate" $
      let res = [ "(KRAKEN, BTC) -- 1000.0 2017-11-01 09:42:23 UTC --> (KRAKEN, USD)"
                , "(KRAKEN, USD) -- 9.0e-4 2017-11-01 09:42:23 UTC --> (KRAKEN, BTC)" ]
          expected = Right ((), OutSync ui1, mempty {_res = res})
      in  runRWST rwst "2017-11-01T09:42:23+00:00 KRAKEN BTC USD 1000.0 0.0009" 
            (OutSync emptyUserInput) `shouldBe` expected
    it "invalid updateRates, it should proceed to findBestRate" $
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
      in  runRWST rwst "KRAKEN BTC KRAKEN USD" outSyncUi2 
            `shouldBe` expected
    it "both updateRates and findBestRate are invalid, it should display all eror message" $
      let err = [ "System unexpecting: \" \""
                , "General error: Invalid timestamp: 2017-11-0109:42:23+00:00"
                , "Invalid request to update rates, probably a request for best rate"
                , "System unexpecting: \"2\""
                , "System unexpecting: \"2\""
                , "Expecting: space"
                , "Expecting: letter" ]
          appStates = [inSyncUi2, outSyncUi2] 
          expected = Right $ appStates <&> ((), , mempty {_err = err})
      in  traverse (runRWST rwst "2017-11-0109:42:23+00:00 KRAKEN BTC USD 1000.0 0.0009") appStates
            `shouldBe` expected
            
updateRatesSpec :: Spec
updateRatesSpec = 
  describe "updateRatesSpec" $ do
    let rwst = updateRates :: RWST String () AppState (Either [String]) ()
    it "orig AppState has empty UserInput, success update should return OutSync with added rates" $
      let expected = Right ((), OutSync ui1, ())
      in  runRWST rwst "2017-11-01T09:42:23+00:00 KRAKEN BTC USD 1000.0 0.0009" (OutSync emptyUserInput) 
            `shouldBe` expected
    it "no matter orig AppState is either InSync or OutSync, success update should return OutSync of added rates" $
      let s = OutSync $ ui1 { _exchRates = updateMap (_exchRates ui1) [gdax_btc_usd, gdax_usd_btc]
                            , _vertices = updateSet (_vertices ui1) [gdax_btc, gdax_usd] }
          expected = Right ((), s, ())
          origStates = [InSync ui1 emptyMatrix, OutSync ui1] 
      in  runRWST rwst "2017-11-01T09:43:23+00:00 GDAX BTC USD 1001.0 0.0008" <$> origStates 
            `shouldBe` replicate 2 expected
    it "update the rate by the newer timestamp no matter the exchange currency cases" $
      let s = OutSync $ ui2 { _exchRates = updateMap (_exchRates ui2)
                              [((kraken_btc, kraken_usd), (1001.1, d2017_11_01t09_42_24)), ((kraken_usd, kraken_btc), (0.00089, d2017_11_01t09_42_24))] }
          expected = Right ((), s, ())
          inputStrings =  [ "2017-11-01T09:42:24+00:00 KRAKEN USD BTC 0.00089 1001.1"
                          , "2017-11-01T09:42:24+00:00 kraken usd btc 0.00089 1001.1"]
      in  (\r -> runRWST rwst r $ OutSync ui2) <$> inputStrings `shouldBe` replicate 2 expected
    it "no matter orig AppState is either InSync or OutSync, AppState remain the same as input ts are not newer" $
      let origStates = [InSync ui1 emptyMatrix, OutSync ui1]
          inputStrings =  [ "2017-11-01T09:42:20+00:00 KRAKEN USD BTC 0.00089 1001.1"
                          , "2017-11-01T09:42:23+00:00 KRAKEN USD BTC 0.00089 1001.1" ] 
          expected = Right . ((), , ()) <$> origStates
      in  nub (runRWST rwst <$> inputStrings <*> origStates) `shouldBe` expected

findBestRateSpec :: Spec
findBestRateSpec = 
  describe "findBestRateSpec" $ do
    let rwst = findBestRate :: RWST String () AppState (Either [String]) RateEntry
    it "failed due to source vertex not exists" $
      runRWST rwst "KRAKEN STC GDAX USD" outSyncUi2
        `shouldBe` Left ["(KRAKEN, STC) is not entered before"]
    it "failed due to dest vertex not exists" $
      runRWST rwst "KRAKEN USD GDAX STC" outSyncUi2
        `shouldBe` Left ["(GDAX, STC) is not entered before"]
    it "no matter orig AppState is InSync or OutSync, it will show the same path and return InSync" $
      let _bestRate = 1001.0
          _start = kraken_btc
          _path = [gdax_btc, gdax_usd, kraken_usd]
          expected = Right (RateEntry{..}, inSyncUi2, ()) 
      in  runRWST rwst "KRAKEN BTC KRAKEN USD" <$> [inSyncUi2, outSyncUi2] 
            `shouldBe` replicate 2 expected


outSyncUi2 :: AppState
outSyncUi2 = OutSync ui2

inSyncUi2 :: AppState
inSyncUi2 = 
  let vertex = V.fromList [gdax_btc, gdax_usd, kraken_btc, kraken_usd]
      matrixOfTuples = 
        [ [(0.0, []), (1001.0, [1]), (1.0, [2]), (1001.0, [1,3])]
        , [(0.0009, [3,2,0]), (0.0, []), (0.0009, [3,2]), (1.0, [3])]
        , [(1.0, [0]), (1001.0, [0,1]), (0.0, []), (1001.0, [0,1,3])]
        , [(0.0009, [2,0]), (1.0, [1]), (0.0009, [2]), (0.0, [])] ]
  in InSync ui2 $ buildRateMatrix vertex matrixOfTuples

ui1 :: UserInput
ui1 = UserInput (M.fromList [kraken_btc_usd, kraken_usd_btc]) (S.fromList [kraken_btc, kraken_usd])

ui2 :: UserInput
ui2 = ui1 { _exchRates = updateMap (_exchRates ui1) [gdax_btc_usd, gdax_usd_btc]
          , _vertices = updateSet (_vertices ui1) [gdax_btc, gdax_usd] }

emptyMatrix :: Matrix RateEntry
emptyMatrix = V.empty -- updateRates doesn't care the matrix value

specs :: [Spec]
specs = [serveReqSpec, findBestRateSpec, updateRatesSpec]
