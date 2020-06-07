{-# LANGUAGE TupleSections
           , FlexibleContexts #-}

module ExchangeRate.ProcessRequests
  ( combineRWST
  , findBestRate
  , updateRates )
  where

import Data.String (String)
import Data.Time
-- import Text.Parsec hiding (State)
import Text.Parsec.String
import Control.Arrow
import Control.Monad.RWS.CPS

import qualified Data.Set as S
import qualified Data.Map as M

import Types
import ExchangeRate.Algorithms
import ExchangeRate.Parsers
import ExchangeRate.Utils

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
findBestRate :: Rwst [String]
findBestRate =
  do s <- get
     let newS@(InSync UserInput{..} m) = syncMatrix s
     put newS
     (\p -> optimumPath p _vertices m) <$> validateExchPair _vertices parseExchPair
   where
     syncMatrix (OutSync ui@UserInput{..}) =
       InSync ui $ floydWarshall 0 . buildMatrix _exchRates . snd . setToMapVector $ _vertices
     syncMatrix inSyncS = inSyncS

-- | Extract the exchange nodes, the corresponding rates and the timestamp from
-- the input string and stores the rates to 'AppState' if the timestamp is newer.
updateRates :: Rwst [String]
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

-- | make sure the given vertice pair from the 'RWST' exist in the given
-- 'Set Vertex'
validateExchPair :: S.Set Vertex ->
                    Rwst (Vertex, Vertex) ->
                    Rwst (Vertex, Vertex)
validateExchPair vertices =
  mapRWST ( >>= (\tuple@((src, dest), _, _) -> exist src >> exist dest >> return tuple) )
  where
    exist v = if S.member v vertices then Right ()
              else Left [show v ++ " is not entered before"]

parseExchPair :: Rwst (Vertex, Vertex)
parseExchPair = genericParse exchPairParser

parseRates :: Rwst (UTCTime, Vertex, Vertex, Double, Double)
parseRates = genericParse exchRatesParser

genericParse :: Parser a -> Rwst a
genericParse p = rwsT (\r s -> fmap (, s, ()) . left parseErrorMsgs $ simpleParse p r)
