module ExchangeRate.Utils
  where

import Protolude
import Prelude (String)
import Data.String.Utils
import Data.List.Split
import Data.Vector as V hiding (length, zip, foldl)
import qualified Data.Map as M
import qualified Data.Set as S

import ExchangeRate.DataTypes

-- | Convert a `Vertex` set to a Vertice vector and a map of the `Vertex` to
-- its index in the `Vector`.  `Vector` is better performed in indexing an
-- element.
setToMapVector :: S.Set Vertex -> (M.Map Vertex Int, V.Vector Vertex)
setToMapVector s = (M.fromList $ zip l [0..(length l)], V.fromList l)
  where l = S.toAscList s

-- | Tokenize a `String` with `" "` as separator
tokenize :: String -> [String]
tokenize = splitOn " " . strip

updateMap :: Ord k => M.Map k v -> [(k, v)] -> M.Map k v
updateMap = foldl (\m (k, v) -> M.insert k v m)

updateSet :: Ord v => S.Set v -> [v] -> S.Set v
updateSet = foldl (\s -> (`S.insert` s))
