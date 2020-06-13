module ProcessRequests
  ( serveReq
  , findBestRate
  , updateRates )
  where

import Control.Lens
import Data.Maybe (isNothing, fromJust)
import Data.List (last)
import Control.Monad.Except (liftEither)
import Control.Monad.Writer

import qualified Data.Map as M

import Types (DisplayMessage(..)
            , AppState(..)
            , RateEntry(..)
            , Vertex(..)
            , AsParseError(..)
            , AsAlgoError(..)
            , ExchRateTimes)
import Algorithms (floydWarshall, optimum)
import Parsers (parseRates, parseExchPair)
import Utils (updateMap)

-- | It doesn't know which request the user is asking for,
-- therefore it call `updateRates` first, if it encounters error, 
-- it will call `findBestRate`, if it still encounters error
-- it will write all the error using `MonadWriter DisplayMessage` that the user doesn't raise any valid request
-- o.w. it will write the success message using `MonadWriter DisplayMessage`.
serveReq
  :: (MonadReader Text m, MonadError e m, AsParseError e, AsAlgoError e, MonadState AppState m, MonadWriter DisplayMessage m)
  => m ()
serveReq = 
  catchError updateRatesM (\err1 ->
    let maybeErr1 = err1 ^? _ParseInputError
        nextReqMsg = "Invalid request to update rates, probably a request for best rate"
    in  when (isJust maybeErr1) $ tell mempty { _err = [fromJust maybeErr1, nextReqMsg] } *> 
          catchError findBestRateM (\err2 ->
            let maybeErr2 = err2 ^? _ParseInputError <|> err2 ^? _AlgoOptimumError
            in  when (isJust maybeErr2) $ tell mempty {_err = [fromJust maybeErr2]}
          )
  )
  where
    -- `updateRatesM` writes the state updated by `updateRates` to `MonadWriter`
    updateRatesM = updateRates *> get >>= \s -> 
        let exRates = exchRateTimesFromState s
            msgs = M.toAscList exRates <&> \((src, dest), (rate, time)) ->
                    tshow src <> " -- " <> tshow rate <> " " <> tshow time <> " --> " <> tshow dest
        in  tell mempty {_res = msgs}
    -- `findBestRateM` writes the `RateEntry` returned by `findBestRate` to `MonadWriter`
    findBestRateM = findBestRate >>= \entry -> tell mempty {_res = presentRateEntry entry}
    presentRateEntry RateEntry{..} = 
      let Vertex srcExch srcCcy = _start
          -- safe to use `last` here because it's returned from `findBestRate` which calls `optimum`
          Vertex destExch destCcy = last _path 
          header = "BEST_RATES_BEGIN " <> 
                    srcExch <> " " <> srcCcy <> " " <> 
                    destExch <> " " <> destCcy <> " " <> 
                    tshow _bestRate
          path = tshow <$> _start : _path
          footer = "BEST_RATES_END"
      in  header : path ++ [footer]

-- | Parse the 2 exchange nodes from the input string
-- get the `AppState` from the state monad, 
-- check if there is an optimised matrix built from the `AppState` user input data,
-- if no, run floyd-warshall for an optimised matrix, 
-- o.w. use the matrix to find the best rate and the exchange nodes involved 
findBestRate 
  :: (MonadReader Text m, MonadError e m, AsParseError e, AsAlgoError e, MonadState AppState m)
  => m RateEntry
findBestRate =
  do
    r <- ask
    (src, dest) <- liftEither $ parseExchPair r
    s <- get
    let (exRates, matrix, isStateChanged) = syncMatrix s
    when isStateChanged (put $ InSync exRates matrix)
    liftEither $ optimum src dest matrix 
    where
      syncMatrix (OutSync exRates) =
        let syncdMatrix = floydWarshall $ M.map fst exRates
        in  (exRates, syncdMatrix, True)
      syncMatrix (InSync exRates syncdMatrix) = (exRates, syncdMatrix, False)

-- | Parse the exchange nodes, the corresponding rates and the timestamp from
-- the input string and stores the rates to `AppState` if the timestamp is newer.
updateRates
  :: (MonadReader Text m, MonadError e m, AsParseError e, MonadState AppState m)
  => m ()
updateRates =
  do
    r <- ask
    (time, src, dest, fwdR, bkdR) <- liftEither $ parseRates r
    exRates <- get <&> exchRateTimesFromState
    let rateOutdated = M.lookup (src, dest) exRates <&> ((< time) . snd)
        updateRequired = isNothing rateOutdated || fromJust rateOutdated
    when updateRequired (put $ newState time src dest fwdR bkdR exRates)
  where
    newState time src dest fwdR bkdR exRates =
      OutSync $ updateMap [((dest, src), (bkdR, time)), ((src, dest), (fwdR, time))] exRates

exchRateTimesFromState :: AppState -> ExchRateTimes
exchRateTimesFromState (InSync exRates _) = exRates
exchRateTimesFromState (OutSync exRates) = exRates