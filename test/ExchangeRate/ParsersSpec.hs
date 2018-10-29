module ExchangeRate.ParsersSpec
  (specs)
  where

import Test.Hspec
import Test.HUnit.Lang

import Protolude
import Prelude (String)
import Control.Arrow
import Data.Time.Clock.POSIX
import Text.Parsec.Error
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Vector as V

import ExchangeRate.DataTypes
import ExchangeRate.Parsers
import ExchangeRate.Utils
import ExchangeRate.Constants

import ExchangeRate.MockData

exchRatesParserSpec :: Spec
exchRatesParserSpec =
  describe "exchRatesParserSpec" $ do
    it "failed due to invalid timestamp" $
      parse "2017-11-01T09:42:3+00:00 KRAKEN BTC USD 1000.0 0.0009"
        `expectParseError` "General error: Invalid timestamp: 2017-11-01T09:42:3+00:00"
    it "failed due to product of both rates bigger > 1" $
      parse "2017-11-01T09:42:23+00:00 KRAKEN BTC USD 1000.0 0.0091"
        `expectParseError` "General error: Product of 1000.0 and 9.1e-3 must be <= 1.0"
    it "failed due to both currencies are the same" $
      parse "2017-11-01T09:42:23+00:00 KRAKEN BTC BTC 1000.0 0.0009"
        `expectParseError` "General error: The currencies must be different"
    it "failed due to both currencies are the same even though they are different cases" $
      parse "2017-11-01T09:42:23+00:00 KRAKEN btc BTC 1000.0 0.0009"
        `expectParseError` "General error: The currencies must be different"
    it "failed due to forward rate is 0" $
      parse "2017-11-01T09:42:23+00:00 KRAKEN BTC USD 0 0.0009"
        `expectParseError` "General error: Rate must be > 0"
    it "failed due to backward rate is < 0" $
      parse "2017-11-01T09:42:23+00:00 KRAKEN BTC USD 1000 -0.0009"
        `expectParseError` "System unexpecting: \"-\""
    it "failed due to forward rate is not numeric" $
      parse "2017-11-01T09:42:23+00:00 KRAKEN BTC USD 1000.0x 0.0009"
        `expectParseError` "System unexpecting: \"x\""
    it "parse valid string" $
      parse "2017-11-01T09:42:23+00:00 KRAKEN BTC USD 1000.0 0.0009" `shouldBe`
        Right (posixSecondsToUTCTime 1509529343, kraken_btc, kraken_usd, 1000.0, 0.0009)
    it "parse valid string ignoring spaces" $
      parse "  2017-11-01T09:42:23+00:00    KRAKEN   BTC  USD   1000.0  0.0009  " `shouldBe`
        Right (posixSecondsToUTCTime 1509529343, kraken_btc, kraken_usd, 1000.0, 0.0009)
    it "convert exchange currency to upper case" $
      parse "2017-11-01T09:42:24+00:00 kraken usd btc 1000.0 0.0009" `shouldBe`
        Right (posixSecondsToUTCTime 1509529344, kraken_usd, kraken_btc, 1000.0, 0.0009)
  where parse = simpleParse exchRatesParser

exchPairParserSpec :: Spec
exchPairParserSpec =
  describe "exchPairParserSpec" $ do
    it "failed due to source and destination are the same" $
      parse "KRAKEN BTC KRAKEN BTC" `expectParseError` "General error: source must be different from destination"
    it "parsed the source and destination" $
      parse "KRAKEN BTC GDAX USD" `shouldBe` Right (kraken_btc, gdax_usd)
    it "parsed the source and destination ignoring all spaces" $
      parse "    KRAKEN BTC   GDAX    USD  " `shouldBe` Right (kraken_btc, gdax_usd)
    it "parsed the source and destination in case insensitve manner" $
      parse "kraken btC Gdax Usd" `shouldBe` Right (kraken_btc, gdax_usd)
  where parse = simpleParse exchPairParser

expectParseError :: Either ParseError a -> String -> Expectation
expectParseError (Left err) s = parseErrorMsgs err `shouldSatisfy` elem s
expectParseError _ s = assertFailure ("Expected failure msg: " ++ s)

specs :: [Spec]
specs = [exchPairParserSpec, exchRatesParserSpec]
