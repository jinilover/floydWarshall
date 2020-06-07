module Main
  (main) where

-- import Protolude hiding (putStrLn, getLine)
-- import Prelude (String, putStrLn, getLine)
import Control.Monad.RWS.CPS
import Data.String (String)

-- import qualified Data.Map as M

-- import ExchangeRate.Utils
-- import ExchangeRate.Parsers
-- import ExchangeRate.Algorithms
import Types
import ExchangeRate.ProcessRequests

main :: IO ()
main = userPrompt $ OutSync emptyUserInput

-- | Prompt for user input, and pass the string to 'updateRates' to process.
-- If it process successfully, it will display the result.  Otherwise,
-- it will pass the same string to 'findBestRate' to process and display the
-- result.  If it still fails, it will inform the user of an invalid string.
-- After processing, it will update its state and wait for next user input.
userPrompt :: AppState -> IO ()
userPrompt s =
  do r <- getLine
     (a, newS, w) <- runRWST combineRWST (toS r) s
     traverse_ putStrLn w
     when (not a) $ putStrLn errMsg
     userPrompt newS
  where
    errMsg :: String
    errMsg = "You neither enter exchange rates or request best rate, please enter a valid input"
