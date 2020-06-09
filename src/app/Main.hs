module Main
  (main) where

import Control.Monad.RWS.CPS
import Data.String (String)

import ProcessRequests (serveReq)
import Types (AppState(..), DisplayMessage(..))
import Utils (emptyUserInput)

main :: IO ()
main = userPrompt $ OutSync emptyUserInput

-- | Prompt for user input, and pass the string to `updateRates` to process.
-- If it process successfully, it will display the result.  Otherwise,
-- it will pass the same string to 'findBestRate to process and display the
-- result.  If it still fails, it will inform the user of an invalid string.
-- After processing, it will provide the most updated state for the next user input.
userPrompt :: AppState -> IO ()
userPrompt s = 
  do
    r <- getLine
    (_, newS, msgs) <- run serveReq (toS r)
    traverse_ putStrLn msgs
    userPrompt newS
  where
    run :: RWST String DisplayMessage AppState (Either [String]) () 
          -> String 
          -> IO ((), AppState, [String])
    run req r = return $ case runRWST req r s of
      Left errs -> ((), s, errs ++ ["\n"])
      Right (_, newS, (DisplayMessage errs [])) -> 
        ((), newS, errs ++ ["You neither enter exchange rates or request best rate, please enter a valid input\n"])
      Right (_, newS, (DisplayMessage _ res)) -> ((), newS, res ++ [""]) 
