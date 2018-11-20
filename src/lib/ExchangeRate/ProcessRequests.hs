{-# LANGUAGE TupleSections #-}
module ExchangeRate.ProcessRequests
  ( combineRWST
  , findBestRate
  , updateRates )
  where

import Protolude
import Prelude (String)
import Data.Time
import Text.Parsec hiding (State)
import Text.Parsec.String
import Control.Arrow
import Control.Monad.RWS.CPS

import qualified Data.Set as S
import qualified Data.Map as M

import ExchangeRate.DataTypes
import ExchangeRate.Algorithms
import ExchangeRate.Parsers
import ExchangeRate.Utils

-- | Combine 'updateRates RWST' and 'findBestRate RWST'.  If the first 'RWST'
-- execution fails, it will execute the second one.
combineRWST :: RWST String [String] AppState IO Bool
combineRWST = transform updateRates >>= (\a ->
    if a then return a
      else tell ["Invalid request to update rates, probably a request for best rate"] >> transform findBestRate
  )
  where
    transform origRWST = do
      r <- ask
      s <- get
      let (a, newS, w) = case runRWST origRWST r s of Left errs -> (False, s, errs)
                                                      Right (msgs, newS, _) -> (True, newS, msgs)
      put newS
      tell w
      return a

-- | Find the best rate and the trades involved for the given exchange nodes
-- (provided by the input string) from the 'AppState' containing
-- the exchange rates between exchange nodes.  It uses floyd-warshall algo
-- to calculate the best rate.
findBestRate :: RWST String () AppState (Either [String]) [String]
findBestRate = do
  s <- get
  let (newS, vertices, m) = newState s
  pair <- validateExchPair vertices parseExchPair
  put newS
  return $ optimumPath pair vertices m
  where
    reoptimize (UserInput rates vertices) =
      floydWarshall 0 . buildMatrix rates . snd . setToMapVector $ vertices

    newState origS@(InSync ui@(UserInput _ vertices) m) = (origS, vertices, m)
    newState (OutSync ui@(UserInput _ vertices)) = (InSync ui newM, vertices, newM)
      where
        newM = reoptimize ui

-- | Extract the exchange nodes, the corresponding rates and the timestamp from
-- the input string and stores the rates to 'AppState' if the timestamp is newer.
updateRates :: RWST String () AppState (Either [String]) [String]
updateRates =
  do (time, src, dest, fwdR, bkdR) <- parseRates
     s <- get
     let (newS, newRates) = updateByTime time src dest fwdR bkdR s $ getUi s
     put newS
     return $ showRates newRates
  where
    getUi (InSync ui _) = ui
    getUi (OutSync ui) = ui

    showRates = map (\((Vertex srcExch srcCcy, Vertex destExch destCcy), (rate, time)) ->
                      "(" ++ srcExch ++ ", " ++ srcCcy ++ ") -- " ++ show rate ++ " " ++ show time ++ " --> (" ++ destExch ++ ", " ++ destCcy ++ ")"
                    ) . M.toAscList

    updateByTime time src dest fwdR bkdR s ui@UserInput{..} =
      maybe (OutSync $ UserInput newRates (updateSet vertices [src, dest]), newRates)
      (\(_, origTime) ->
        if origTime < time then (OutSync ui { exchRates = newRates }, newRates) else (s, exchRates)
      ) $
      M.lookup (src, dest) exchRates
        where
          newRates = updateMap exchRates [((dest, src), (bkdR, time)), ((src, dest), (fwdR, time))]

-- | make sure the given vertice pair from the 'RWST' exist in the given
-- 'Set Vertex'
validateExchPair :: S.Set Vertex ->
                    RWST String () AppState (Either [String]) (Vertex, Vertex) ->
                    RWST String () AppState (Either [String]) (Vertex, Vertex)
validateExchPair vertices =
  mapRWST (>>= (\pair@((src, dest), _, _) -> exist src >> exist dest >> return pair))
  where
    exist v@Vertex{..} = if S.member v vertices then Right ()
                         else Left ["(" ++ exch ++ ", " ++ ccy ++ ")" ++ " is not entered before"]

parseExchPair :: RWST String () AppState (Either [String]) (Vertex, Vertex)
parseExchPair = parseString exchPairParser

parseRates :: RWST String () AppState (Either [String]) (UTCTime, Vertex, Vertex, Double, Double)
parseRates = parseString exchRatesParser

parseString :: Parser a -> RWST String () AppState (Either [String]) a
parseString p = rwsT (\r s -> (, s, ()) <$> left parseErrorMsgs (simpleParse p r))
