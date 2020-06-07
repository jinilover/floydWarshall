module ExchangeRate.Algorithms
  where

-- import Protolude hiding (maybeToEither)
import Data.String (String)
-- import Data.Either.Utils
import Data.List.HT

import Data.Vector as V hiding ((++))

import qualified Data.Map as M
import qualified Data.Set as S

import ExchangeRate.DataTypes
import ExchangeRate.Constants
import ExchangeRate.Utils

-- | Build a matrix of n*n size where n is the size of the `Vertex` vector
-- Each entry is filled with the rate between the Vertex if there is any
-- available from the given `ExchRates` map.  This matrix will be applied then
-- floydWarshall algorithm
buildMatrix :: ExchRates -> V.Vector Vertex -> Matrix
buildMatrix exRates v = updateRow <$> seqNums
  where
    seqNums = V.fromList [0 .. V.length v - 1]
    updateRow i = updateCol <$> seqNums
      where
        updateCol j
          | i == j = emptyEntry
          | ccy vtxI == ccy vtxJ = MatrixEntry 1 [j]
          | otherwise = maybe emptyEntry (flip MatrixEntry [j] . fst) $ M.lookup (vtxI, vtxJ) exRates
          where
            vtxI = v ! i
            vtxJ = v ! j

-- | The floydWarshall algorithm
floydWarshall :: Int -> Matrix -> Matrix
floydWarshall k matrix
  | k < matrixSize = floydWarshall (k + 1) $ updateRow <$> seqNums
  | otherwise = matrix
  where
    matrixSize = V.length matrix
    seqNums = V.fromList [0 .. matrixSize - 1]
    updateRow i
      | i == k = matrix ! k
      | otherwise = updateCol <$> seqNums
      where
        updateCol j
          | i == j || k == j = matrix ! i ! j
          | origRate < newRate = MatrixEntry newRate (ikPath ++ kjPath)
          | otherwise = origEntry
          where
            origEntry@(MatrixEntry origRate _) = matrix ! i ! j

            MatrixEntry ikRate ikPath = matrix ! i ! k
            MatrixEntry kjRate kjPath = matrix ! k ! j

            newRate = ikRate * kjRate

-- | Find the best rate and the path to be taken for the given vertice pair.
-- And convert the information into output message.
optimumPath :: (Vertex, Vertex) -> S.Set Vertex -> Matrix -> [String]
optimumPath (src, dest) set m = either (: []) identity $ do
  srcIdx <- maybeToEither (show src ++ " is not entered before") $ M.lookup src vertexIndexMap
  destIdx <- maybeToEither (show dest ++ " is not entered before") $ M.lookup dest vertexIndexMap
  let entry@MatrixEntry{..} = m ! srcIdx ! destIdx
  maybeToEither "Not reachable" $ prettyShow bestRate <$> srcToDest entry
  where
    srcToDest MatrixEntry{..} =
      let pathVertices = (vertices !) <$> path in
      viewR pathVertices >>= (\(_, end) -> if end == dest then Just (src:pathVertices) else Nothing)

    (vertexIndexMap, vertices) = setToMapVector set

    prettyShow rate xs =
      ("BEST_RATES_BEGIN " ++
        exch src ++ " " ++
        ccy src ++ " " ++
        exch dest ++ " " ++
        ccy dest ++ " " ++
        show rate) :
        (showVertex <$> xs) ++
        ["BEST_RATES_END"]
