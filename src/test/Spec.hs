import Test.Hspec

import qualified ExchangeRate.UtilsSpec as Utils
import qualified ExchangeRate.ParsersSpec as Parsers
import qualified ExchangeRate.AlgorithmsSpec as Algorithms
import qualified ExchangeRate.ProcessRequestsSpec as ProcessRequests

main :: IO ()
main = hspec $ foldl (>>) (return ()) specs

specs :: [Spec]
specs = Utils.specs ++ Parsers.specs ++ Algorithms.specs ++ ProcessRequests.specs
