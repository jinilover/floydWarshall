module ParserTest 
  ( test_Parser )
  where

import Data.String (String)
import Data.Time.Clock.POSIX

import Hedgehog
import Test.Tasty
import Test.Tasty.Hedgehog

import Parsers (parseRates, parseExchPair)

import MockData (kraken_btc, kraken_usd, gdax_usd)

test_Parser :: TestTree
test_Parser = testGroup "Parser"
  [ testGroup "parseRates" 
    [ testGroup "invalid input" 
      [ testProperty "invalid timestamp" parseRates_invalidTimestamp
      , testProperty "rate product > 1" parseRates_productBiggerThan1
      , testProperty "same currencies" parseRates_sameCurrencies
      , testProperty "same currencies ignore case" parseRates_sameCurrenciesIgnoreCase
      , testProperty "0 forward rate" parseRates_zeroFwdRate 
      , testProperty "-ve backward rate" parseRates_negativeBkdRate 
      , testProperty "invalid forward rate" parseRates_invalidFwdRate ] 
    , testGroup "valid input"
      [ testProperty "valid string" parseRates_validString 
      , testProperty "valid string spaces ignored" parseRates_ignoreSpace
      , testProperty "valid string currency to upper case" parseRates_ccyToUpperCase ]
    ]
  , testGroup "parseExchPair" 
    [ testGroup "invalid input" 
      [ testProperty "same source and destination" parseExchPair_sameSrcDest ]
    , testGroup "valid input" 
      [ testProperty "valid string" parseExchPair_validString
      , testProperty "valid string spaces ignored" parseExchPair_ignoreSpace
      , testProperty "valid string ignore case" parseExchPair_ignoreCase ]
    ]
  ]

parseRates_invalidTimestamp :: Property
parseRates_invalidTimestamp = property do
  parseRates "2017-11-01T09:42:3+00:00 KRAKEN BTC USD 1000.0 0.0009"
    `expectParseError` "General error: Invalid timestamp: 2017-11-01T09:42:3+00:00"

parseRates_productBiggerThan1 :: Property
parseRates_productBiggerThan1 = property do
  parseRates "2017-11-01T09:42:23+00:00 KRAKEN BTC USD 1000.0 0.0091"
    `expectParseError` "General error: Product of 1000.0 and 9.1e-3 must be <= 1.0"

parseRates_sameCurrencies :: Property
parseRates_sameCurrencies = property do
  parseRates "2017-11-01T09:42:23+00:00 KRAKEN BTC BTC 1000.0 0.0009"
    `expectParseError` "General error: The currencies must be different"

parseRates_sameCurrenciesIgnoreCase :: Property
parseRates_sameCurrenciesIgnoreCase = property do
  parseRates "2017-11-01T09:42:23+00:00 KRAKEN btc BTC 1000.0 0.0009"
    `expectParseError` "General error: The currencies must be different"

parseRates_zeroFwdRate :: Property
parseRates_zeroFwdRate = property do
  parseRates "2017-11-01T09:42:23+00:00 KRAKEN BTC USD 0 0.0009"
    `expectParseError` "General error: Rate must be > 0"

parseRates_negativeBkdRate :: Property
parseRates_negativeBkdRate = property do
  parseRates "2017-11-01T09:42:23+00:00 KRAKEN BTC USD 1000 -0.0009"
    `expectParseError` "System unexpecting: \"-\""

parseRates_invalidFwdRate :: Property
parseRates_invalidFwdRate = property do
  parseRates "2017-11-01T09:42:23+00:00 KRAKEN BTC USD 1000.0x 0.0009"
    `expectParseError` "System unexpecting: \"x\""

parseRates_validString :: Property
parseRates_validString = property do
  parseRates "2017-11-01T09:42:23+00:00 KRAKEN BTC USD 1000.0 0.0009" ===
    Right (posixSecondsToUTCTime 1509529343, kraken_btc, kraken_usd, 1000.0, 0.0009)

parseRates_ignoreSpace :: Property
parseRates_ignoreSpace = property do
  parseRates "  2017-11-01T09:42:23+00:00    KRAKEN   BTC  USD   1000.0  0.0009  " ===
    Right (posixSecondsToUTCTime 1509529343, kraken_btc, kraken_usd, 1000.0, 0.0009)

parseRates_ccyToUpperCase :: Property
parseRates_ccyToUpperCase = property do
  parseRates "2017-11-01T09:42:24+00:00 kraken usd btc 1000.0 0.0009" ===
    Right (posixSecondsToUTCTime 1509529344, kraken_usd, kraken_btc, 1000.0, 0.0009)

parseExchPair_sameSrcDest :: Property
parseExchPair_sameSrcDest = property do
  parseExchPair "KRAKEN BTC KRAKEN BTC" `expectParseError` 
    "General error: source must be different from destination"

parseExchPair_validString :: Property
parseExchPair_validString = property do 
  parseExchPair "KRAKEN BTC GDAX USD" === Right (kraken_btc, gdax_usd)
 
parseExchPair_ignoreSpace :: Property
parseExchPair_ignoreSpace = property do
  parseExchPair "    KRAKEN BTC   GDAX    USD  " === Right (kraken_btc, gdax_usd)

parseExchPair_ignoreCase :: Property 
parseExchPair_ignoreCase = property do
  parseExchPair "kraken btC Gdax Usd" === Right (kraken_btc, gdax_usd)

expectParseError :: MonadTest m => Either [String] a -> String -> m ()
expectParseError (Left errs) s = assert $ elem s errs
expectParseError _ _ = failure
