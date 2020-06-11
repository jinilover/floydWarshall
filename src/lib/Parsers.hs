module Parsers
  ( parseExchPair
  , parseRates)
  where

import Prelude hiding (option)
import Control.Lens((#))
import Control.Monad.Except (liftEither)
import Control.Monad.Fail (fail)
import Data.Attoparsec.Text ( Parser
                            , skipSpace
                            , parseOnly
                            , many1
                            , satisfy
                            , letter
                            , double )
import Data.Text
import Data.Time (UTCTime, parseTimeM, defaultTimeLocale)

import Types (Vertex(..), AsParseError(..))

-- | Parse the string for updating exchange rates.  It extracts timestamp,
-- exchange, source currency, destination currency, forward rate and backward
-- rate from the string by using parsec functions
exchRatesParser :: Parser (UTCTime, Vertex, Vertex, Double, Double)
exchRatesParser = do
  tS <- skipSpace >> many1 (satisfy (/= ' '))
  time <- parseTimestamp tS
  exchS <- skipSpace >> alphabets
  srcS <- skipSpace >> alphabets
  destS <- skipSpace >> alphabets
  fwdR <- skipSpace >> double >>= positiveCheck
  bkdR <- skipSpace >> double >>= positiveCheck
  when (fwdR * bkdR > 1.0) $ fail ("Product of " ++ show fwdR ++ " and " ++ show bkdR ++ " must be <= 1.0")
  [exch, src, dest] <- return $ fmap toUpper [exchS, srcS, destS]
  when (src == dest) $ fail "The currencies must be different"
  return (time, Vertex exch src, Vertex exch dest, fwdR, bkdR)
  where
    parseTimestamp = parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%S%z"
    positiveCheck r = if r <= 0 then fail "Rate must be > 0" else return r

-- | Parse the string to a pair of vertices.  These vertices are later on used
-- for requesting best rate.  It extracts source exchange, source currency,
-- destination exchange and destination currency from the string by using parsec
-- functions.
exchPairParser :: Parser (Vertex, Vertex)
exchPairParser = do
  srcExch' <- skipSpace >> alphabets
  srcCcy' <- skipSpace >> alphabets
  destExch' <- skipSpace >> alphabets
  destCcy' <- skipSpace >> alphabets
  [srcExch, srcCcy, destExch, destCcy] <- return $ fmap toUpper [srcExch', srcCcy', destExch', destCcy']
  let pair@(src, dest) = (Vertex srcExch srcCcy, Vertex destExch destCcy)
  when (src == dest) $ fail "source must be different from destination"
  return pair

alphabets :: Parser Text
alphabets = many1 letter <&> toS

parseRates 
  :: (MonadError e m, AsParseError e)
  => Text -> m (UTCTime, Vertex, Vertex, Double, Double)
parseRates = simpleParse exchRatesParser

parseExchPair 
  :: (MonadError e m, AsParseError e)
  => Text -> m (Vertex, Vertex)
parseExchPair = simpleParse exchPairParser

simpleParse
  :: (MonadError e m, AsParseError e)
  => Parser a -> Text -> m a
simpleParse p = liftEither . first ((_ParseInputError #) . toS) . parseOnly p