{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module Types where

import Data.Time
import Data.String (String)
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

-- | Rate entry composes of the best rate, the starting vertex 
-- and the path taken to achieve that rate.  
data RateEntry = 
  RateEntry {
    _bestRate :: Double
  , _start :: Vertex
  , _path :: [Vertex]
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
data AppState = InSync UserInput (Matrix RateEntry)
              | OutSync UserInput
              deriving (Show, Eq)

type Matrix a = V.Vector (V.Vector a)

type ExchRates = M.Map (Vertex, Vertex) (Double, UTCTime)

-- type Rwst a = RWST String () AppState (Either [String]) a

data DisplayMessage =
  DisplayMessage {
    _findBestRateErr :: [String]
  , _updateRatesErr :: [String]
  , _findBestRateRes :: [String]
  , _updateRatesRes :: [String]
  }

instance Semigroup DisplayMessage where
  (DisplayMessage findErr1 updateErr1 findRes1 updateRes1) <> 
    (DisplayMessage findErr2 updateErr2 findRes2 updateRes2) = 
    DisplayMessage {
      _findBestRateErr = findErr1 <> findErr2
    , _updateRatesErr = updateErr1 <> updateErr2
    , _findBestRateRes = findRes1 <> findRes2
    , _updateRatesRes = updateRes1 <> updateRes2
    }

instance Semigroup DisplayMessage => Monoid DisplayMessage where
  mempty = DisplayMessage [] [] [] []

emptyUserInput :: UserInput
emptyUserInput = UserInput M.empty S.empty