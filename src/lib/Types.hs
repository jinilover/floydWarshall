{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}

module Types where

import Control.Lens
import Data.Time

import qualified Data.Map as M
import qualified Data.Vector as V

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

type ExchRateTimes = M.Map (Vertex, Vertex) (Double, UTCTime)

-- | AppState which represents whether the matrix of the best rates are in-sync
-- with `ExchRateTimes` collected from the user input .  This applies the idea of FSM.
data AppState = InSync ExchRateTimes (Matrix RateEntry)
              | OutSync ExchRateTimes
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

data ParseError = ParseInputError Text 
                  deriving (Show, Eq)

makeClassyPrisms ''ParseError

newtype AlgoError = AlgoOptimumError Text
                    deriving (Show, Eq)

makeClassyPrisms ''AlgoError

data AppError = AppParseError ParseError
              | AppAlgoError AlgoError
              deriving (Show, Eq)

makeClassyPrisms ''AppError

instance AsParseError AppError where
  _ParseError = _AppParseError

instance AsAlgoError AppError where
  _AlgoError = _AppAlgoError
