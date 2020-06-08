module ExchangeRate.Algorithms
  ( buildMatrix
  , floydWarshall
  , optimum
  )
  where

import Data.String (String)

import Data.Vector as V hiding ((++), any, null)

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
  | k < matrixSize = let newMatrix = indices <&> updateRow in runAlgo (k + 1) newMatrix
  | otherwise = matrix
  where
    matrixSize = V.length matrix
    indices = V.fromList [0 .. matrixSize - 1]
    updateRow rowNum
      | rowNum == k = matrix ! k
      | otherwise = indices <&> updateCol 
      where
        updateCol colNum
          | any (== colNum) [rowNum, k] = origEntry
          | _bestRate origEntry < newRate = origEntry {_bestRate = newRate, _path = ikPath ++ kjPath}
          | otherwise = origEntry
          where
            origEntry = matrix ! rowNum ! colNum
            RateEntry ikRate _ ikPath = matrix ! rowNum ! k
            RateEntry kjRate _ kjPath = matrix ! k ! colNum
            newRate = ikRate * kjRate

-- | Return the best rate and the path to be taken 
-- for the provided `src` and `dest` vertices if it exists
optimum :: Vertex -> Vertex -> S.Set Vertex -> Matrix RateEntry -> Either String (Double, [Vertex])
optimum src dest set matrix = 
  do
    srcIdx <- maybeToEither (show src ++ " is not entered before") $ indexOfElem src set
    destIdx <- maybeToEither (show dest ++ " is not entered before") $ indexOfElem dest set
    let RateEntry{..} = matrix ! srcIdx ! destIdx
    if null _path 
      then Left "Not reachable" 
      else Right (_bestRate, _path)