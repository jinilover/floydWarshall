{-# LANGUAGE TupleSections
           , FlexibleContexts #-}

module ExchangeRate.ProcessRequests
  ( combineRWST
  , findBestRate
  , findBestRate'
  , updateRates
  , updateRates' )
  where

import Data.Maybe (isNothing, fromJust)
import Data.String (String)
import Data.Time
import Text.Parsec.String
import Control.Arrow
import Control.Monad.RWS.CPS
import Control.Monad.Except (liftEither)

import qualified Data.Set as S
import qualified Data.Map as M

import Types
import ExchangeRate.Algorithms
import ExchangeRate.Parsers
import ExchangeRate.Utils (updateMap, updateSet, setToMapVector)

-- | Combine 'updateRates RWST' and 'findBestRate RWST'.  If the first 'RWST'
-- execution fails, it will execute the second one.
combineRWST :: RWST String [String] AppState IO Bool
combineRWST = executeRWST updateRates >>= nextRwst
  where
    executeRWST rwst = mapRWST (return . runIdentity) $
      rws (\r s -> case runRWST rwst r s of Left errs -> (False, s, errs)
                                            Right (msgs, newS, _) -> (True, newS, msgs))

    nextRwst False = tell ["Invalid request to update rates, probably a request for best rate"]
                     >> executeRWST findBestRate
    nextRwst _ = return True

-- | Find the best rate and the trades involved for the given exchange nodes
-- (provided by the input string) from the 'AppState' containing
-- the exchange rates between exchange nodes.  It uses floyd-warshall algo
-- to calculate the best rate.
findBestRate :: RWST String () AppState (Either [String]) [String]
findBestRate =
  do s <- get
     let (ui@UserInput{..}, m) = syncMatrix s
     put $ InSync ui m
     (src, dest) <- validateExchPair _vertices parseExchPair
     return . present src dest $ optimum src dest _vertices m
   where
     syncMatrix (OutSync ui@UserInput{..}) =
       let  syncdMatrix = floydWarshall . buildMatrix _exchRates . snd . setToMapVector $ _vertices
       in   (ui, syncdMatrix)
     syncMatrix (InSync ui syncdMatrix) = (ui,syncdMatrix)
     present _ _ (Left err) = [err]
     present src@(Vertex srcExch srcCcy) (Vertex destExch destCcy) (Right (rate, vertices)) = 
       let  header = "BEST_RATES_BEGIN " ++ 
                      srcExch ++ " " ++ srcCcy ++ " " ++ 
                      destExch ++ " " ++ destCcy ++ " " ++ 
                      show rate
            path = show <$> src : vertices       
            footer = "BEST_RATES_END"
       in   header : path ++ [footer]

-- | Parse the 2 exchange nodes from the input string
-- get the `AppState` from the state monad, 
-- check if there is an optimised matrix built from the `AppState` user input data,
-- if no, run floyd-warshall for an optimised matrix, 
-- o.w. use the matrix to find the best rate and the exchange nodes involved 
findBestRate' 
  :: (MonadReader String m, MonadError String m, MonadState AppState m)
  => m (Double, [Vertex])
findBestRate' =
  do
    r <- ask
    (src, dest) <- liftEither $ parseExchPair' r
    s <- get
    let (ui@UserInput{..}, matrix, isStateChanged) = syncMatrix s
    when isStateChanged (put $ InSync ui matrix)
    liftEither $ optimum src dest _vertices matrix <&> (<&> (src :))
    where
      syncMatrix (OutSync ui@UserInput{..}) =
        let syncdMatrix = floydWarshall . buildMatrix _exchRates . snd . setToMapVector $ _vertices
        in  (ui, syncdMatrix, True)
      syncMatrix (InSync ui syncdMatrix) = (ui,syncdMatrix, False)

-- | Extract the exchange nodes, the corresponding rates and the timestamp from
-- the input string and stores the rates to 'AppState' if the timestamp is newer.
updateRates :: RWST String () AppState (Either [String])  [String]
updateRates =
  do (time, src, dest, fwdR, bkdR) <- parseRates
     state (\s -> let newS = updateState time src dest fwdR bkdR s in (showRates . _exchRates . extractUi $ newS, newS))
  where
    extractUi (InSync ui _) = ui
    extractUi (OutSync ui) = ui

    showRates = map showRate . M.toAscList

    showRate ((src, dest), (rate, time)) =
      show src ++ " -- " ++ show rate ++ " " ++ show time ++ " --> " ++ show dest

    updateState time src dest fwdR bkdR s =
      maybe insert updateByTime . M.lookup (src, dest) . _exchRates $ ui
      where
        ui = extractUi s
        insert = OutSync . UserInput newRates . flip updateSet [src, dest] . _vertices $ ui
        updateByTime (_, origTime) = if origTime < time then OutSync ui { _exchRates = newRates} else s
        newRates = updateMap (_exchRates ui)
                   [ ((dest, src), (bkdR, time))
                   , ((src, dest), (fwdR, time)) ]

updateRates'
  :: (MonadReader String m, MonadError String m, MonadState AppState m)
  => m [String]
updateRates' =
  do
    r <- ask
    (time, src, dest, fwdR, bkdR) <- liftEither $ parseRates' r
    ui@UserInput{..} <- get <&> uiFromState
    let rateOutdated = M.lookup (src, dest) _exchRates <&> ((< time) . snd)
        updateRequired = isNothing rateOutdated || fromJust rateOutdated
    when updateRequired (put $ newState time src dest fwdR bkdR ui)
    ui' <- get <&> uiFromState
    return $ showRates ui'
  where
    uiFromState (InSync ui _) = ui
    uiFromState (OutSync ui) = ui
    newState time src dest fwdR bkdR UserInput{..} = 
      let newExchRates = updateMap _exchRates [((dest, src), (bkdR, time)), ((src, dest), (fwdR, time))]
          newVertices = updateSet _vertices [src, dest]
      in  OutSync $ UserInput newExchRates newVertices
    showRates UserInput{..} = 
      M.toAscList _exchRates <&> \((src, dest), (rate, time)) ->
        show src ++ " -- " ++ show rate ++ " " ++ show time ++ " --> " ++ show dest

-- | make sure the given vertice pair from the 'RWST' exist in the given
-- 'Set Vertex'
-- TODO S.Set Vertex comes from the state, it is not needed
-- this validation is not needed because `optimum` call will check it
validateExchPair :: S.Set Vertex ->
                    RWST String () AppState (Either [String]) (Vertex, Vertex) ->
                    RWST String () AppState (Either [String]) (Vertex, Vertex)
validateExchPair vertices =
  mapRWST ( >>= (\tuple@((src, dest), _, _) -> exist src >> exist dest >> return tuple) )
  where
    exist v = if S.member v vertices then Right ()
              else Left [show v ++ " is not entered before"]

-- TODO remove the following 3 functions
parseExchPair :: RWST String () AppState (Either [String]) (Vertex, Vertex)
parseExchPair = genericParse exchPairParser

parseRates :: RWST String () AppState (Either [String]) (UTCTime, Vertex, Vertex, Double, Double)
parseRates = genericParse exchRatesParser

genericParse :: Parser a -> RWST String () AppState (Either [String]) a
genericParse p = rwsT (\r s -> fmap (, s, ()) . left parseErrorMsgs $ simpleParse p r)

