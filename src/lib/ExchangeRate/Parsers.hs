module ExchangeRate.Parsers
  where

import Protolude hiding ((<|>), maybeToEither, many)
import Prelude (String)
import Text.Read
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Error
import Control.Monad
import Control.Arrow
import Data.Char
import Data.Time
import Data.Either.Utils

import qualified Data.Map as M
import qualified Data.Set as S

import ExchangeRate.DataTypes
import ExchangeRate.Utils

-- | Parse the tokens for updating exchange rates.  It parses the given tokens,
--  `[String]`, to timestamp, exchange, source currency, destination currency,
-- forward rate and backward rate respectively.  The tokens are valid, it will
-- update the existing `UserInput` data and return the new `UserInput`.
-- Otherwise, it will return a failure reason.
exchRatesParser :: Parser (UTCTime, Vertex, Vertex, Double, Double)
exchRatesParser = do
  tS <- skipSpaces >> many1 (satisfy (/= ' '))
  time <- maybe (parserFail ("Invalid timestamp: " ++ tS)) return $ parseTime tS
  exchS <- skipSpaces >> alphabets
  srcS <- skipSpaces >> alphabets
  destS <- skipSpaces >> alphabets
  fwdR <- skipSpaces >> decimal
  bkdR <- skipSpaces >> decimal
  when (fwdR * bkdR > 1.0) $ parserFail ("Product of " ++ show fwdR ++ " and " ++ show bkdR ++ " must be <= 1.0")
  [exch, src, dest] <- return $ map toUpper <$> [exchS, srcS, destS]
  when (src == dest) $ parserFail "The currencies must be different"
  return (time, Vertex exch src, Vertex exch dest, fwdR, bkdR)
  where decimal :: ParsecT String () Identity Double
        decimal = fmap read ((++) <$> integer <*> mantissa) >>= positive
        mantissa = option "" $ (:) <$> char '.' <*> integer
        integer = many1 digit
        parseTime :: String -> Maybe UTCTime
        parseTime = parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%S%z"
        positive r = if r <= 0 then parserFail "Rate must be > 0" else return r

-- | Parse the tokens to a pair of vertices.  These vertices are later on used
-- for requesting best rate.  It parses the given tokens, `[String]`, to
-- source exchange, source currency, destination exchange and destination
-- currency respectively.  The vertice pair will be returned only after
-- successful validation.  Otherwise, it will return a failure reason.
exchPairParser :: Parser (Vertex, Vertex)
exchPairParser = do
  srcExch' <- skipSpaces >> alphabets
  srcCcy' <- skipSpaces >> alphabets
  destExch' <- skipSpaces >> alphabets
  destCcy' <- skipSpaces >> alphabets
  [srcExch, srcCcy, destExch, destCcy] <- return $ map toUpper <$> [srcExch', srcCcy', destExch', destCcy']
  pair@(src, dest) <- return (Vertex srcExch srcCcy, Vertex destExch destCcy)
  when (src == dest) $ parserFail "source must be different from destination"
  return pair

alphabets :: ParsecT String () Identity String
alphabets = many1 letter

skipSpaces :: ParsecT String () Identity ()
skipSpaces = skipMany space

simpleParse :: Parser a -> String -> Either ParseError a
simpleParse = (`parse` "regularParse")

parserErrorMsgs :: ParseError -> [String]
parserErrorMsgs = map interpret . errorMessages
  where interpret (SysUnExpect s) = "System unexpecting: " ++ s
        interpret (UnExpect s) = "Unexpecting: " ++ s
        interpret (Expect s) = "Expecting: " ++ s
        interpret (Message s) = "General error: " ++ s
