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
combineRWST = transformRWST updateRates >>= (\a ->
    if a then return a
      else tell ["Invalid request to update rates, probably a request for best rate"] >> transformRWST findBestRate
  )
  where
    transformRWST rwst = do
      r <- ask
      s <- get
      let (a, newS, w) = case runRWST rwst r s of Left errs -> (False, s, errs)
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
  let (newS, vertices, m) = syncMatrix s
  pair <- validateExchPair vertices parseExchPair
  put newS
  return $ optimumPath pair vertices m
  where
    reoptimize (UserInput rates vertices) =
      floydWarshall 0 . buildMatrix rates . snd . setToMapVector $ vertices

    syncMatrix origS@(InSync UserInput{..} m) = (origS, vertices, m)
    syncMatrix (OutSync ui@UserInput{..}) = let newM = reoptimize ui in
                                            (InSync ui newM, vertices, newM)

-- | Extract the exchange nodes, the corresponding rates and the timestamp from
-- the input string and stores the rates to 'AppState' if the timestamp is newer.
updateRates :: RWST String () AppState (Either [String]) [String]
updateRates =
  do (time, src, dest, fwdR, bkdR) <- parseRates
     s <- get
     let newS = updateState time src dest fwdR bkdR s
     put newS
     return . showRates . exchRates . userInput $ newS
  where
    userInput (InSync ui _) = ui
    userInput (OutSync ui) = ui

    showRates = map showRate . M.toAscList

    showRate ((src, dest), (rate, time)) =
      "(" ++ showVertex src ++ ") -- " ++ show rate ++ " " ++ show time ++ " --> (" ++ showVertex dest ++ ")"

    updateState time src dest fwdR bkdR s =
      maybe insert update . M.lookup (src, dest) . exchRates $ ui
      where
        ui = userInput s
        insert = OutSync . UserInput newRates . (`updateSet` [src, dest]) . vertices $ ui
        update (_, origTime) = if origTime < time then OutSync ui { exchRates = newRates} else s
        newRates = updateMap (exchRates ui) [((dest, src), (bkdR, time)), ((src, dest), (fwdR, time))]

-- | make sure the given vertice pair from the 'RWST' exist in the given
-- 'Set Vertex'
validateExchPair :: S.Set Vertex ->
                    RWST String () AppState (Either [String]) (Vertex, Vertex) ->
                    RWST String () AppState (Either [String]) (Vertex, Vertex)
validateExchPair vertices =
  mapRWST (>>= (\tuple@(pair, _, _) -> exist (fst pair) >> exist (snd pair) >> return tuple))
  where
    exist v = if S.member v vertices then Right ()
              else Left ["(" ++ showVertex v ++ ")" ++ " is not entered before"]

parseExchPair :: RWST String () AppState (Either [String]) (Vertex, Vertex)
parseExchPair = parseString exchPairParser

parseRates :: RWST String () AppState (Either [String]) (UTCTime, Vertex, Vertex, Double, Double)
parseRates = parseString exchRatesParser

parseString :: Parser a -> RWST String () AppState (Either [String]) a
parseString p = rwsT (\r s -> fmap (, s, ()) . left parseErrorMsgs $ simpleParse p r)
