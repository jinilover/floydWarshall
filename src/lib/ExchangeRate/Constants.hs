module ExchangeRate.Constants
  where

import ExchangeRate.DataTypes

import qualified Data.Set as S
import qualified Data.Map as M

emptyUserInput :: UserInput
emptyUserInput = UserInput M.empty S.empty

emptyEntry :: MatrixEntry
emptyEntry = MatrixEntry 0 []
