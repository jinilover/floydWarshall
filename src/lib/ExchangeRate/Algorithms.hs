module ExchangeRate.Algorithms
  ( buildMatrix
  , floydWarshall
  , optimumPath 
  )
  where

-- import Protolude hiding (maybeToEither)
import Data.String (String)
-- import Data.Either.Utils
import Data.List.HT

import Data.Vector as V hiding ((++), any)

import qualified Data.Map as M
import qualified Data.Set as S

import Types
import ExchangeRate.Utils (emptyEntry, indexOfElem)

-- | Build a matrix of n*n size where n is the size of the `Vertex` vector
-- Each entry is filled with the rate between the Vertex if there is any
-- available from the given `ExchRates` map.  This matrix will be applied the
-- floydWarshall algorithm
buildMatrix :: ExchRates -> V.Vector Vertex -> Matrix RateEntry
buildMatrix exRates vertices = buildRow <$> indices
  where
    indices = V.fromList [0 .. V.length vertices - 1]
    buildRow rowNum = let defaultEntry = emptyEntry (vertices ! rowNum) in buildCol defaultEntry <$> indices
      where
        buildCol defaultEntry@(RateEntry _ vtxI _) colNum
          | rowNum == colNum = defaultEntry
          | _ccy vtxI == _ccy vtxJ = defaultEntry {_bestRate = 1.0, _path = [vtxJ]}
          | otherwise = case M.lookup (vtxI, vtxJ) exRates of
                          Just (rate, _) -> defaultEntry {_bestRate = rate, _path = [vtxJ]}
                          _ -> defaultEntry
          where
            vtxJ = vertices ! colNum

-- | The floydWarshall algorithm
floydWarshall :: Matrix RateEntry -> Matrix RateEntry
floydWarshall = runAlgo 0

runAlgo :: Int -> Matrix RateEntry -> Matrix RateEntry
runAlgo k matrix -- k counts the number of iteration
  | k < matrixSize = runAlgo (k + 1) (indices <&> updateRow)
  | otherwise = matrix
  where
    matrixSize = V.length matrix
    indices = V.fromList [0 .. matrixSize - 1]
    updateRow rowNum
      | rowNum == k = matrix ! k
      | otherwise = indices <&> updateCol 
      where
        updateCol colNum
          | any (== colNum) [rowNum, k] = matrix ! rowNum ! colNum
          | _bestRate origEntry < newRate = origEntry {_bestRate = newRate, _path = ikPath ++ kjPath}
          | otherwise = origEntry
          where
            origEntry = matrix ! rowNum ! colNum
            RateEntry ikRate _ ikPath = matrix ! rowNum ! k
            RateEntry kjRate _ kjPath = matrix ! k ! colNum
            newRate = ikRate * kjRate

-- | Find the best rate and the path to be taken for the given vertice pair.
-- And convert the information into output message.
optimumPath :: (Vertex, Vertex) -> S.Set Vertex -> Matrix RateEntry -> [String]
optimumPath (src, dest) set m = either (\err -> [err]) identity $ do
  srcIdx <- maybeToEither (show src ++ " is not entered before") $ indexOfElem src set
  destIdx <- maybeToEither (show dest ++ " is not entered before") $ indexOfElem dest set
  let entry@RateEntry{..} = m ! srcIdx ! destIdx
  maybeToEither "Not reachable" $ prettyShow _bestRate <$> srcToDest entry
  where
    srcToDest RateEntry{..} = viewR _path >>= \(_, end) -> 
      if end == dest then Just (src : _path) else Nothing -- TODO try not to `src : _path`
    prettyShow rate xs =
      ("BEST_RATES_BEGIN " ++
        _exch src ++ " " ++
        _ccy src ++ " " ++
        _exch dest ++ " " ++
        _ccy dest ++ " " ++
        show rate) :
        (show <$> xs) ++
        ["BEST_RATES_END"]
