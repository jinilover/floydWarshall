module Algorithms
  ( buildMatrix
  , floydWarshall
  , optimum
  )
  where

import Data.Vector as V hiding ((++), any, null)

import qualified Data.Map as M

import Types (RateEntry(..), Matrix, Vertex(..))
import Utils (isolatedEntry)

-- | Build a matrix of n*n size where n is the size of the `Vertex` vector
-- Each entry is filled with the rate between the Vertex if there is any
-- available from the given map.  This matrix will be applied the
-- floydWarshall algorithm
buildMatrix :: M.Map (Vertex, Vertex) Double -> V.Vector Vertex -> Matrix RateEntry
buildMatrix exRates vertices = indices <&> buildRow
  where
    indices = V.fromList [0 .. V.length vertices - 1]
    buildRow i = let entry = isolatedEntry (vertices ! i) in indices <&> buildCol entry
      where
        buildCol entry@(RateEntry _ vtxI _) j
          | i == j = entry -- i is row number, j is column number
          | _ccy vtxI == _ccy vtxJ = entry {_bestRate = 1.0, _path = [vtxJ]}
          | otherwise = case M.lookup (vtxI, vtxJ) exRates of
                          Just rate -> entry {_bestRate = rate, _path = [vtxJ]}
                          _ -> entry
          where
            vtxJ = vertices ! j

-- | The floydWarshall algorithm
floydWarshall :: Matrix RateEntry -> Matrix RateEntry
floydWarshall = runAlgo 0

runAlgo :: Int -> Matrix RateEntry -> Matrix RateEntry
runAlgo k matrix -- k is the number of times to run this algo
  | k < matrixSize = let newMatrix = indices <&> updateRow in runAlgo (k + 1) newMatrix
  | otherwise = matrix
  where
    matrixSize = V.length matrix
    indices = V.fromList [0 .. matrixSize - 1]
    updateRow i
      | i == k = matrix ! k
      | otherwise = indices <&> updateCol 
      where
        updateCol j
          | any (== j) [i, k] = origEntry
          | _bestRate origEntry < newRate = origEntry {_bestRate = newRate, _path = ikPath ++ kjPath}
          | otherwise = origEntry
          where
            origEntry = matrix ! i ! j
            RateEntry ikRate _ ikPath = matrix ! i ! k
            RateEntry kjRate _ kjPath = matrix ! k ! j
            newRate = ikRate * kjRate

-- | Return the best rate and the path 
-- for the provided `src` and `dest` vertices if it exists
optimum :: Vertex -> Vertex -> Matrix RateEntry -> Either Text RateEntry
optimum src dest matrix = 
  do
    when (null matrix || any null matrix) (Left "The matrix is empty")
    let verticalVector = matrix <&> (_start . (! 0))
    srcIdx <- maybeToEither (tshow src <> " is not entered before") $ V.elemIndex src verticalVector
    destIdx <- maybeToEither (tshow dest <> " is not entered before") $ V.elemIndex dest verticalVector
    let entry@RateEntry{..} = matrix ! srcIdx ! destIdx
    if null _path 
      then Left $ "There is no exchange between " <> tshow src <> " and " <> tshow dest 
      else Right entry