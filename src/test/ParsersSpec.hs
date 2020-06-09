module ParsersSpec
  (specs)
  where

import Test.Hspec
import Test.HUnit.Lang

import Data.String (String)
import Data.Time.Clock.POSIX

import Parsers (parseRates, parseExchPair)

import MockData (kraken_btc, kraken_usd, gdax_usd)

exchRatesParserSpec :: Spec
exchRatesParserSpec =
  describe "exchRatesParserSpec" $ do
    it "failed due to invalid timestamp" $
      parseRates "2017-11-01T09:42:3+00:00 KRAKEN BTC USD 1000.0 0.0009"
        `expectParseError` "General error: Invalid timestamp: 2017-11-01T09:42:3+00:00"
    it "failed due to product of both rates bigger > 1" $
      parseRates "2017-11-01T09:42:23+00:00 KRAKEN BTC USD 1000.0 0.0091"
        `expectParseError` "General error: Product of 1000.0 and 9.1e-3 must be <= 1.0"
    it "failed due to both currencies are the same" $
      parseRates "2017-11-01T09:42:23+00:00 KRAKEN BTC BTC 1000.0 0.0009"
        `expectParseError` "General error: The currencies must be different"
    it "failed due to both currencies are the same even though they are different cases" $
      parseRates "2017-11-01T09:42:23+00:00 KRAKEN btc BTC 1000.0 0.0009"
        `expectParseError` "General error: The currencies must be different"
    it "failed due to forward rate is 0" $
      parseRates "2017-11-01T09:42:23+00:00 KRAKEN BTC USD 0 0.0009"
        `expectParseError` "General error: Rate must be > 0"
    it "failed due to backward rate is < 0" $
      parseRates "2017-11-01T09:42:23+00:00 KRAKEN BTC USD 1000 -0.0009"
        `expectParseError` "System unexpecting: \"-\""
    it "failed due to forward rate is not numeric" $
      parseRates "2017-11-01T09:42:23+00:00 KRAKEN BTC USD 1000.0x 0.0009"
        `expectParseError` "System unexpecting: \"x\""
    it "parse valid string" $
      parseRates "2017-11-01T09:42:23+00:00 KRAKEN BTC USD 1000.0 0.0009" `shouldBe`
        Right (posixSecondsToUTCTime 1509529343, kraken_btc, kraken_usd, 1000.0, 0.0009)
    it "parse valid string ignoring spaces" $
      parseRates "  2017-11-01T09:42:23+00:00    KRAKEN   BTC  USD   1000.0  0.0009  " `shouldBe`
        Right (posixSecondsToUTCTime 1509529343, kraken_btc, kraken_usd, 1000.0, 0.0009)
    it "convert exchange currency to upper case" $
      parseRates "2017-11-01T09:42:24+00:00 kraken usd btc 1000.0 0.0009" `shouldBe`
        Right (posixSecondsToUTCTime 1509529344, kraken_usd, kraken_btc, 1000.0, 0.0009)

exchPairParserSpec :: Spec
exchPairParserSpec =
  describe "exchPairParserSpec" $ do
    it "failed due to source and destination are the same" $
      parseExchPair "KRAKEN BTC KRAKEN BTC" `expectParseError` "General error: source must be different from destination"
    it "parsed the source and destination" $
      parseExchPair "KRAKEN BTC GDAX USD" `shouldBe` Right (kraken_btc, gdax_usd)
    it "parsed the source and destination ignoring all spaces" $
      parseExchPair "    KRAKEN BTC   GDAX    USD  " `shouldBe` Right (kraken_btc, gdax_usd)
    it "parsed the source and destination in case insensitve manner" $
      parseExchPair "kraken btC Gdax Usd" `shouldBe` Right (kraken_btc, gdax_usd)

expectParseError :: Either [String] a -> String -> Expectation
expectParseError (Left errs) s = errs `shouldSatisfy` elem s
expectParseError _ s = assertFailure ("Expected failure msg: " ++ s)

specs :: [Spec]
specs = [exchPairParserSpec, exchRatesParserSpec]
