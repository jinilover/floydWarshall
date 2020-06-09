{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}

module ProcessRequests
  ( serveReq
  , findBestRate
  , updateRates )
  where

import Data.Maybe (isNothing, fromJust)
import Data.List (last)
import Data.String (String)
import Control.Monad.Except (liftEither)
import Control.Monad.Writer

import qualified Data.Map as M

import Types
import Algorithms
import Parsers
import Utils (updateMap, updateSet, setToVector)

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
            msgs = M.toAscList _exchRates <&> \((src, dest), (rate, time)) ->
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
        let syncdMatrix = floydWarshall . buildMatrix _exchRates . setToVector $ _vertices
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
    let rateOutdated = M.lookup (src, dest) _exchRates <&> ((< time) . snd)
        updateRequired = isNothing rateOutdated || fromJust rateOutdated
    when updateRequired (put $ newState time src dest fwdR bkdR ui)
  where
    newState time src dest fwdR bkdR UserInput{..} = 
      let newExchRates = updateMap _exchRates [((dest, src), (bkdR, time)), ((src, dest), (fwdR, time))]
          newVertices = updateSet _vertices [src, dest]
      in  OutSync $ UserInput newExchRates newVertices

userInputFromState :: AppState -> UserInput
userInputFromState (InSync ui _) = ui
userInputFromState (OutSync ui) = ui