module Parsers
  ( exchRatesParser
  , exchPairParser
  , parseErrorMsgs'
  , parseExchPair'
  , parseRates')
  where

import Prelude hiding (option)
import Data.String (String)
import Text.Read
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Error
import Data.Char
import Data.Time hiding (parseTime)

import Types

-- | Parse the string for updating exchange rates.  It extracts timestamp,
-- exchange, source currency, destination currency, forward rate and backward
-- rate from the string by using parsec functions
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
  where
    decimal = read <$> ((++) <$> integer <*> mantissa) >>= positive
    mantissa = option "" $ (:) <$> char '.' <*> integer
    integer = many1 digit
    parseTime = parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%S%z"
    positive r = if r <= 0 then parserFail "Rate must be > 0" else return r

-- | Parse the string to a pair of vertices.  These vertices are later on used
-- for requesting best rate.  It extracts source exchange, source currency,
-- destination exchange and destination currency from the string by using parsec
-- functions.
exchPairParser :: Parser (Vertex, Vertex)
exchPairParser = do
  srcExch' <- skipSpaces >> alphabets
  srcCcy' <- skipSpaces >> alphabets
  destExch' <- skipSpaces >> alphabets
  destCcy' <- skipSpaces >> alphabets
  [srcExch, srcCcy, destExch, destCcy] <- return $ map toUpper <$> [srcExch', srcCcy', destExch', destCcy']
  let pair@(src, dest) = (Vertex srcExch srcCcy, Vertex destExch destCcy)
  when (src == dest) $ parserFail "source must be different from destination"
  return pair

alphabets :: Parser String
alphabets = many1 letter

skipSpaces :: Parser ()
skipSpaces = skipMany space

parseErrorMsgs' :: ParseError -> [String]
parseErrorMsgs' = map interpret . errorMessages
  where
    interpret (SysUnExpect s) = "System unexpecting: " ++ s
    interpret (UnExpect s) = "Unexpecting: " ++ s
    interpret (Expect s) = "Expecting: " ++ s
    interpret (Message s) = "General error: " ++ s

parseRates' :: String -> Either [String] (UTCTime, Vertex, Vertex, Double, Double)
parseRates' = parseOnly exchRatesParser

parseExchPair' :: String -> Either [String] (Vertex, Vertex)
parseExchPair' = parseOnly exchPairParser

parseOnly :: Parser a -> String -> Either [String] a
parseOnly p = first parseErrorMsgs' . parse p "regularParse"