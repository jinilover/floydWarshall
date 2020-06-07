module ExchangeRate.TestUtils
  where

import qualified Data.Vector as V

import Types

tuplesToMatrix :: [[(Double, [Int])]] -> Matrix
tuplesToMatrix = V.fromList . (V.fromList . (uncurry MatrixEntry <$>) <$>)
