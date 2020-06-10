module Utils
  where

import Data.Vector as V hiding (length, zip, foldl, (++))
import qualified Data.Map as M
import qualified Data.Set as S

import Types (Vertex, RateEntry(..), UserInput(..), AppState(..))

-- | Convert a `Vertex` set to a Vertice vector and a map of the `Vertex` to
-- its index in the `Vector`.  `Vector` is better performed in indexing an
-- element.
setToVector :: S.Set Vertex -> V.Vector Vertex
setToVector = V.fromList . S.toAscList

updateMap :: Ord k => [(k, v)] -> M.Map k v -> M.Map k v
updateMap = flip $ foldl (\m (k, v) -> M.insert k v m)

updateSet :: Ord v => [v] -> S.Set v -> S.Set v
updateSet = flip . foldl . flip $ S.insert

-- | Create a `RateEntry` for a vertex that has no exchange rate 
-- with any other exchange yet
isolatedEntry :: Vertex -> RateEntry
isolatedEntry start = RateEntry 0.0 start []

blankState :: AppState
blankState = OutSync $ UserInput M.empty S.empty