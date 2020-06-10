import Test.Tasty

import UtilsTest
import ParserTest
import ProcessRequestsTest
import AlgorithmsTest

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "Unit Tests"
    [ test_Utils
    , test_Parser
    , test_ProcessRequests
    , test_Algorithms ]