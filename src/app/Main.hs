module Main
  (main) where

import Protolude hiding (putStrLn, getLine)
import Prelude (String, putStrLn, getLine)
import Control.Monad.RWS.CPS

import qualified Data.Map as M

import ExchangeRate.Utils
import ExchangeRate.Parsers
import ExchangeRate.Algorithms
import ExchangeRate.DataTypes
import ExchangeRate.Constants
import ExchangeRate.ProcessRequests

main :: IO ()
main = userPrompt $ OutSync emptyUserInput

-- | Prompt for user input and tokenizing the input strings into tokens.
-- The tokens are expected to be either exchange rates update or best rate
-- request.

userPrompt :: AppState -> IO ()
userPrompt s =
  do r <- getLine
     (a, newS, w) <- runRWST combineRWST r s
     traverse putStrLn w
     when (not a) $ putStrLn "You neither enter exchange rates or request best rate, please enter a valid input"
     userPrompt newS
