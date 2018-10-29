module ExchangeRate.Algorithms
  where

import Protolude hiding (maybeToEither)
import Prelude (String)
import Data.Either.Utils
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
  where seqNums = V.fromList [0..V.length v - 1]
        updateRow i = updateCol <$> seqNums
          where updateCol j
                  | i == j = emptyEntry
                  | otherwise =
                    let [vtxI, vtxJ] = (v !) <$> [i, j] in
                    if ccy vtxI == ccy vtxJ then MatrixEntry 1 [j]
                    else
                      maybe emptyEntry ((`MatrixEntry` [j]) . fst) $
                        M.lookup (vtxI, vtxJ) exRates

-- | The floydWarshall algorithm
floydWarshall :: Int -> Matrix -> Matrix
floydWarshall k matrix
  | k < matrixSize = floydWarshall (k + 1) $ updateRow <$> seqNums
  | otherwise = matrix
  where matrixSize = V.length matrix
        seqNums = V.fromList [0..matrixSize - 1]
        updateRow i
          | i == k = matrix ! k
          | otherwise = updateCol <$> seqNums
          where updateCol j
                  | i == j || k == j = matrix ! i ! j
                  | otherwise = if origRate < newRate then MatrixEntry newRate (ikPath ++ kjPath)
                                else origEntry
                  where origEntry@(MatrixEntry origRate _) = matrix ! i ! j
                        MatrixEntry ikRate ikPath = matrix ! i ! k
                        MatrixEntry kjRate kjPath = matrix ! k ! j
                        newRate = ikRate * kjRate

-- | Find the best rate and the path to be taken for the given vertice pair.
-- And convert the information into output message.
optimumPath :: (Vertex, Vertex) -> S.Set Vertex -> Matrix -> [String]
optimumPath (src, dest) set m =
  let result =
        let (vertexToInt, vertices) = setToMapVector set in
        do [srcIdx, destIdx] <- maybeToEither
                                "Both source and destination must have been entered before" $
                                traverse (`M.lookup` vertexToInt) [src, dest]
           MatrixEntry{..} <- return $ m ! srcIdx ! destIdx
           let p = viewR ((vertices !) <$> path) >>=
                    (\(xs, x) -> if x == dest then Just (src : xs ++ [x]) else Nothing)
           maybe (Left "Not reachable") (Right . interpret bestRate) p in
  either (: []) identity result
  where interpret r xs =
          ("BEST_RATES_BEGIN " ++ exch src ++ " " ++ ccy src ++ " " ++ exch dest ++ " " ++ ccy dest ++ " " ++ show r) :
            ((\x -> exch x ++ ", " ++ ccy x) <$> xs) ++ ["BEST_RATES_END"]
