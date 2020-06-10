{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}

module ProcessRequests
  ( serveReq
  , findBestRate
  , updateRates )
  where

import Control.Lens
import Data.Maybe (isNothing, fromJust)
import Data.List (last)
import Data.String (String)
import Control.Monad.Except (liftEither)
import Control.Monad.Writer

import qualified Data.Map as M

import Types (DisplayMessage(..)
            , AppState(..)
            , RateEntry(..)
            , UserInput(..)
            , exchRateTimes
            , vertices
            , Vertex(..))
import Algorithms (floydWarshall, buildMatrix, optimum)
import Parsers (parseRates, parseExchPair)
import Utils (updateMap, updateSet, setToVector)

-- | It doesn't know which request the user is asking for,
-- therefore it call `updateRates` first, if it encounters error, 
-- it will call `findBestRate`, if it still encounters error
-- it will write all the error using `MonadWriter DisplayMessage` that the user doesn't raise any valid request
-- o.w. it will write the success message using `MonadWriter DisplayMessage`.
serveReq
  :: (MonadReader String m, MonadError [String] m, MonadState AppState m, MonadWriter DisplayMessage m)
  => m ()
serveReq = 
  catchError updateRatesM \errs -> 
    tell mempty {_err = errs ++ ["Invalid request to update rates, probably a request for best rate"]} *> 
      catchError findBestRateM \stillErrs -> tell mempty {_err = stillErrs}
  where
    updateRatesM = updateRates *> get >>= \s -> 
        let UserInput{..} = userInputFromState s
            msgs = M.toAscList _exchRateTimes <&> \((src, dest), (rate, time)) ->
                    show src ++ " -- " ++ show rate ++ " " ++ show time ++ " --> " ++ show dest
        in  tell mempty {_res = msgs}
    findBestRateM = findBestRate >>= \entry -> tell mempty {_res = presentRateEntry entry}
    presentRateEntry RateEntry{..} = 
      let Vertex srcExch srcCcy = _start
          Vertex destExch destCcy = last _path
          header = "BEST_RATES_BEGIN " ++ 
                    srcExch ++ " " ++ srcCcy ++ " " ++ 
                    destExch ++ " " ++ destCcy ++ " " ++ 
                    show _bestRate
          path = show <$> _start : _path
          footer = "BEST_RATES_END"
      in  header : path ++ [footer]

-- | Parse the 2 exchange nodes from the input string
-- get the `AppState` from the state monad, 
-- check if there is an optimised matrix built from the `AppState` user input data,
-- if no, run floyd-warshall for an optimised matrix, 
-- o.w. use the matrix to find the best rate and the exchange nodes involved 
findBestRate 
  :: (MonadReader String m, MonadError [String] m, MonadState AppState m)
  => m RateEntry
findBestRate =
  do
    r <- ask
    (src, dest) <- liftEither $ parseExchPair r
    s <- get
    let (ui@UserInput{..}, matrix, isStateChanged) = syncMatrix s
    when isStateChanged (put $ InSync ui matrix)
    liftEither . first return $ optimum src dest (setToVector _vertices) matrix 
    where
      syncMatrix (OutSync ui@UserInput{..}) =
        let vector = setToVector _vertices
            exchRates = M.map fst _exchRateTimes
            syncdMatrix = floydWarshall $ buildMatrix exchRates vector
        in  (ui, syncdMatrix, True)
      syncMatrix (InSync ui syncdMatrix) = (ui,syncdMatrix, False)

-- | Parse the exchange nodes, the corresponding rates and the timestamp from
-- the input string and stores the rates to `AppState` if the timestamp is newer.
updateRates
  :: (MonadReader String m, MonadError [String] m, MonadState AppState m)
  => m ()
updateRates =
  do
    r <- ask
    (time, src, dest, fwdR, bkdR) <- liftEither $ parseRates r
    ui@UserInput{..} <- get <&> userInputFromState
    let rateOutdated = M.lookup (src, dest) _exchRateTimes <&> ((< time) . snd)
        updateRequired = isNothing rateOutdated || fromJust rateOutdated
    when updateRequired (put $ newState time src dest fwdR bkdR ui)
  where
    newState time src dest fwdR bkdR ui =
      OutSync $ ui & exchRateTimes %~ updateMap [((dest, src), (bkdR, time)), ((src, dest), (fwdR, time))]
                   & vertices %~ updateSet [src, dest]

userInputFromState :: AppState -> UserInput
userInputFromState (InSync ui _) = ui
userInputFromState (OutSync ui) = ui