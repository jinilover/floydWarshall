module ExchangeRate.TestUtils
  where

import Data.Vector as V

import Types

listsToMatrix :: [[a]] -> Matrix a
listsToMatrix = V.fromList . (fmap V.fromList)

createRateMatrix :: V.Vector Vertex -> Matrix (Double, [Int]) -> Matrix RateEntry
createRateMatrix vertices matrix = 
  V.zip vertices matrix <&> \(vertex, row) ->
    row <&> \(rate, indices) -> 
      let _bestRate = rate
          _start = vertex
          _path = indices <&> (vertices !)
      in  RateEntry{..}

buildRateMatrix :: V.Vector Vertex -> [[(Double, [Int])]] -> Matrix RateEntry
buildRateMatrix vertices = createRateMatrix vertices . listsToMatrix