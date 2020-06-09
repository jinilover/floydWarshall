import Test.Hspec

import qualified UtilsSpec as Utils
import qualified ParsersSpec as Parsers
import qualified AlgorithmsSpec as Algorithms
import qualified ProcessRequestsSpec as ProcessRequests

main :: IO ()
main = hspec $ foldl (>>) (return ()) specs

specs :: [Spec]
specs = Utils.specs ++ Parsers.specs ++ Algorithms.specs ++ ProcessRequests.specs
