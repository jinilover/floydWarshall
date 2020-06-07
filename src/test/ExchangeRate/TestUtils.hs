module ExchangeRate.TestUtils
  where

import Protolude
import qualified Data.Vector as V

import ExchangeRate.DataTypes

tuplesToMatrix :: [[(Double, [Int])]] -> Matrix
tuplesToMatrix = V.fromList . (V.fromList . (uncurry MatrixEntry <$>) <$>)
