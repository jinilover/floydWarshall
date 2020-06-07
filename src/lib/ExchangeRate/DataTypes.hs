module ExchangeRate.DataTypes
  where

import Data.Time
import Data.String (String)
import Control.Monad.RWS.CPS
import qualified Data.Map as M
import qualified Data.Vector as V
import qualified Data.Set as S

-- | `Vertex` composes of the exchange and currency
data Vertex = Vertex {
                exch :: String
              , ccy :: String} deriving (Ord, Eq)

instance Show Vertex where
  show Vertex{..} = "(" ++ exch ++ ", " ++ ccy ++ ")"

-- | Matrix entry composes of the best rate, and the path taken to
-- achieve it.  It contains numbers that represent the vertices and
-- used as the matrice indices.
-- e.g. if path is [0, 3], the vertex i should be exchanged via vertex 0 and
-- then vertex 3 to get the best rate with vertex 3
data MatrixEntry = MatrixEntry {
                  bestRate :: Double
                , path :: [Int]
                } deriving (Show, Eq)

-- | Contains the most updated exchange rates between vertices
-- and all vertices involved as provided by user input
data UserInput = UserInput {
            exchRates :: ExchRates
          , vertices :: S.Set Vertex
          } deriving (Show, Eq)

-- | AppState which represents whether the matrix of the best rates are in-sync
-- with the UserInput.  This applies the idea of FSM.
data AppState = InSync UserInput Matrix
              | OutSync UserInput
              deriving (Show, Eq)

type Matrix = V.Vector (V.Vector MatrixEntry)

type ExchRates = M.Map (Vertex, Vertex) (Double, UTCTime)

type Rwst a = RWST String () AppState (Either [String]) a
