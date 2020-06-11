{-# LANGUAGE UndecidableInstances #-}

module Types where

import Control.Lens

import Data.Time

import qualified Data.Map as M
import qualified Data.Vector as V
import qualified Data.Set as S

-- | `Vertex` composes of the exchange and currency
data Vertex = 
  Vertex {
    _exch :: Text
  , _ccy :: Text
  } deriving (Ord, Eq)

instance Show Vertex where
  show Vertex{..} = toS $ "(" <> _exch <> ", " <> _ccy <> ")"

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
    _exchRateTimes :: M.Map (Vertex, Vertex) (Double, UTCTime)
  , _vertices :: S.Set Vertex
  } deriving (Show, Eq)

$(makeLenses ''UserInput)

-- | AppState which represents whether the matrix of the best rates are in-sync
-- with the UserInput.  This applies the idea of FSM.
data AppState = InSync UserInput (Matrix RateEntry)
              | OutSync UserInput
              deriving (Show, Eq)

type Matrix a = V.Vector (V.Vector a)

-- | Used by the MonadWriter
-- therefore it's a monoid
data DisplayMessage =
  DisplayMessage {
    _err :: [Text]
  , _res :: [Text]
  } deriving (Show, Eq)

instance Semigroup DisplayMessage where
  (DisplayMessage err1 res1) <> (DisplayMessage err2 res2) = 
    DisplayMessage {
      _err = err1 <> err2
    , _res = res1 <> res2
    }

instance Semigroup DisplayMessage => Monoid DisplayMessage where
  mempty = DisplayMessage [] []