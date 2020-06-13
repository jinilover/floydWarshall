module Utils
  where

import qualified Data.Map as M

import Types (Vertex, RateEntry(..), AppState(..))

updateMap :: Ord k => [(k, v)] -> M.Map k v -> M.Map k v
updateMap = flip $ foldl (\m (k, v) -> M.insert k v m)

-- | Create a `RateEntry` for a vertex that has no exchange rate 
-- with any other exchange yet
isolatedEntry :: Vertex -> RateEntry
isolatedEntry start = RateEntry 0.0 start []

blankState :: AppState
blankState = OutSync M.empty