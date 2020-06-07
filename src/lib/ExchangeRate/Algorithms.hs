module ExchangeRate.Algorithms
  where

-- import Protolude hiding (maybeToEither)
import Data.String (String)
-- import Data.Either.Utils
import Data.List.HT

import Data.Vector as V hiding ((++))

import qualified Data.Map as M
import qualified Data.Set as S

import Types
import ExchangeRate.Utils

-- | Build a matrix of n*n size where n is the size of the `Vertex` vector
-- Each entry is filled with the rate between the Vertex if there is any
-- available from the given `ExchRates` map.  This matrix will be applied the
-- floydWarshall algorithm
buildMatrix :: ExchRates -> V.Vector Vertex -> Matrix RateEntry
buildMatrix exRates v = updateRow <$> indices
  where
    indices = V.fromList [0 .. V.length v - 1]
    updateRow i = updateCol <$> indices
      where
        updateCol j
          | i == j = emptyEntry
          | ccyI == ccyJ = RateEntry 1 [j]
          | otherwise = maybe emptyEntry (flip RateEntry [j] . fst) $ M.lookup (vtxI, vtxJ) exRates
          where
            vtxI@(Vertex _ ccyI) = v ! i
            vtxJ@(Vertex _ ccyJ) = v ! j

-- | The floydWarshall algorithm
floydWarshall :: Int -> Matrix RateEntry -> Matrix RateEntry -- TODO remove Int
floydWarshall k matrix
  | k < matrixSize = floydWarshall (k + 1) $ updateRow <$> indices
  | otherwise = matrix
  where
    matrixSize = V.length matrix
    indices = V.fromList [0 .. matrixSize - 1]
    updateRow i
      | i == k = matrix ! k
      | otherwise = updateCol <$> indices
      where
        updateCol j
          | i == j || k == j = matrix ! i ! j
          | origRate < newRate = RateEntry newRate (ikPath ++ kjPath)
          | otherwise = origEntry
          where
            origEntry@(RateEntry origRate _) = matrix ! i ! j

            RateEntry ikRate ikPath = matrix ! i ! k
            RateEntry kjRate kjPath = matrix ! k ! j

            newRate = ikRate * kjRate

-- | Find the best rate and the path to be taken for the given vertice pair.
-- And convert the information into output message.
optimumPath :: (Vertex, Vertex) -> S.Set Vertex -> Matrix RateEntry -> [String]
optimumPath (src, dest) set m = either (: []) identity $ do
  srcIdx <- maybeToEither (show src ++ " is not entered before") $ M.lookup src vertexIndexMap
  destIdx <- maybeToEither (show dest ++ " is not entered before") $ M.lookup dest vertexIndexMap
  let entry@RateEntry{..} = m ! srcIdx ! destIdx
  maybeToEither "Not reachable" $ prettyShow _bestRate <$> srcToDest entry
  where
    srcToDest RateEntry{..} =
      let pathVertices = (vertices !) <$> _path 
      in viewR pathVertices >>= (\(_, end) -> if end == dest then Just (src:pathVertices) else Nothing)

    (vertexIndexMap, vertices) = setToMapVector set

    prettyShow rate xs =
      ("BEST_RATES_BEGIN " ++
        _exch src ++ " " ++
        _ccy src ++ " " ++
        _exch dest ++ " " ++
        _ccy dest ++ " " ++
        show rate) :
        (show <$> xs) ++
        ["BEST_RATES_END"]
