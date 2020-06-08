module ExchangeRate.Utils
  where

-- import Data.String.Utils
-- import Data.List.Split
import Data.Vector as V hiding (elemIndex, length, zip, foldl, (++))
import Data.List (elemIndex)
import qualified Data.Map as M
import qualified Data.Set as S

import Types

-- | Convert a `Vertex` set to a Vertice vector and a map of the `Vertex` to
-- its index in the `Vector`.  `Vector` is better performed in indexing an
-- element.
-- TODO reconsider the return type
setToMapVector :: S.Set Vertex -> (M.Map Vertex Int, V.Vector Vertex)
setToMapVector s = (M.fromList $ zip l [0 .. length l], V.fromList l)
  where
    l = S.toAscList s

indexOfElem :: Eq a => a -> S.Set a -> Maybe Int
indexOfElem a = elemIndex a . S.toAscList

updateMap :: Ord k => M.Map k v -> [(k, v)] -> M.Map k v
updateMap = foldl (\m (k, v) -> M.insert k v m)

updateSet :: Ord v => S.Set v -> [v] -> S.Set v
updateSet = foldl . flip $ S.insert

-- | Create a `RateEntry` for a vertex that has no exchange rate 
-- with any other exchange yet
isolatedEntry :: Vertex -> RateEntry
isolatedEntry start = RateEntry 0.0 start []