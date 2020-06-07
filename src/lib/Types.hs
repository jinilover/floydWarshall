module Types where

import Data.Time
import Data.String (String)
import Control.Monad.RWS.CPS
import qualified Data.Map as M
import qualified Data.Vector as V
import qualified Data.Set as S

-- | `Vertex` composes of the exchange and currency
data Vertex = 
  Vertex {
    _exch :: String
  , _ccy :: String
  } deriving (Ord, Eq)

instance Show Vertex where
  show Vertex{..} = "(" ++ _exch ++ ", " ++ _ccy ++ ")"

-- | Matrix entry composes of the best rate, and the path taken to
-- achieve it.  It contains numbers that represent the vertices and
-- used as the matrice indices.
-- e.g. if path is [0, 3], the vertex i should be exchanged via vertex 0 and
-- then vertex 3 to get the best rate with vertex 3
data MatrixEntry = 
  MatrixEntry {
    _bestRate :: Double
  , _path :: [Int]
  } deriving (Show, Eq)

-- | Contains the most updated exchange rates between vertices
-- and all vertices involved as provided by user input
data UserInput = 
  UserInput {
    _exchRates :: ExchRates
  , _vertices :: S.Set Vertex
  } deriving (Show, Eq)

-- | AppState which represents whether the matrix of the best rates are in-sync
-- with the UserInput.  This applies the idea of FSM.
data AppState = InSync UserInput (Matrix MatrixEntry)
              | OutSync UserInput
              deriving (Show, Eq)

type Matrix a = V.Vector (V.Vector a)

type ExchRates = M.Map (Vertex, Vertex) (Double, UTCTime)

type Rwst a = RWST String () AppState (Either [String]) a

emptyUserInput :: UserInput
emptyUserInput = 
  let _exchRates = M.empty
      _vertices = S.empty
  in  UserInput{..}

emptyEntry :: MatrixEntry
emptyEntry = MatrixEntry 0 []