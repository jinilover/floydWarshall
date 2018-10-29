# Solving the problem of exchange rate graph

## Background
The user input the exchange rates between 2 currencies in an exchange.  A graph is formed by vertices of exchange/currency pair where the edges connecting the vertices are exchange rates.  The goal is locating the path of hops (trades) between vertices that provides the best rate.  The idea is similar to locating the shortest path.

### User input for exchange rates
User input the rates in the following format
```
<timestamp> <exchange> <source_currency> <destination_currency> <forward_rate> <backward_rate>
```
E.g.
```
2017-11-01T09:42:23+00:00 KRAKEN BTC USD 1000.0 0.0009
```
It means at Kraken, 1 BTC can be traded for 1000.0 USD and 1 USD can be traded for 0.0009

Continuous entering rates will form a graph.
E.g.
```
2017-11-01T09:42:23+00:00 KRAKEN BTC USD 1000.0 0.0009
2017-11-01T09:43:23+00:00 GDAX BTC USD 1001.0 0.0008
```
For the same currency across different exchanges, 1.0 is assigned to the connecting edge.  The following graph is formed.

![Alt text](https://github.com/jinilover/fsm-rwst-parsec-floydWarshall/blob/master/GraphOfExchanges.png)

### User input for the best rate
The input format is:
```
<source_exchange> <source_currency> <destination_exchange> <destination_currency>
```
E.g. 
```
KRAKEN BTC GDAX USD
```
The system is expected to return
```
BEST_RATES_BEGIN KRAKEN BTC GDAX USD 1001.1
KRAKEN BTC
GDAX BTC
GDAX USD
BEST_RATES_END
```

## Algebraic Data Types
* `Vertex` composes of the exchange and currency.
* `MatrixEntry` contains the rate between the vertices and the path connecting them.
* `UserInput` to hold the most updated rates between vertices provided by the user
* `Matrix`
* `AppState` which is either `InSync` or `OutSync`.  It applies the FSM concept.  `InSync` means the matrix is optimized with the latest `UserInput`.  When the `UserInput` is updated with the price update request, the matrix is no longer valid such that it should update `AppState` as `OutSync`.  In processing the best rate request, it uses the `AppState` to decide whether it should re-run the algorithm to optimize the matrix.  If it is `InSync`, it will not waste the time to re-run the algorithm.  Otherwise, it will re-run the algorithm and update `AppState` as `InSync`.

## Build application
* Under `fsm-rwst-parsec-floydWarshall` folder, run `stack build`

## Run application
* Under `fsm-rwst-parsec-floydWarshall` folder, run `stack exec fsm-rwst-parsec-floydWarshall`
* Please refer to the "Appendix" section for details.

## Run unit-tests
* Under `fsm-rwst-parsec-floydWarshall` folder, run `stack build fsm-rwst-parsec-floydWarshall\:test\:fsm-rwst-parsec-floydWarshall-test`, it should run the tests as follows

```
setToMapVectorSpec
  converts an empty Set
  converts a Vertex Set to (Map Vertex Int, Vector Vectex) pair
exchPairParserSpec
  failed due to source and destination are the same
  parsed the source and destination
  parsed the source and destination ignoring all spaces
  parsed the source and destination in case insensitve manner
exchRatesParserSpec
  failed due to invalid timestamp
  failed due to product of both rates bigger > 1
  failed due to both currencies are the same
  failed due to both currencies are the same even though they are different cases
  failed due to forward rate is 0
  failed due to backward rate is < 0
  failed due to forward rate is not numeric
  parse valid string
  parse valid string ignoring spaces
  convert exchange currency to upper case
buildMatrixSpec
  can handle no vertex
  build a matrix expected to be inputMatrix1 4*4
  build a matrix expected to be inputMatrix2 7*7
floydWarshallSpec
  floydWarshall can handle empty matrix
  floydWarshall on inputMatrix1 4*4
  floydWarshall on inputMatrix2 7*7
optimumPathSpec
  Source not exists in the graph
  Destination not exists in the graph
  Source cannot reach destination in the graph
  Reachable from kraken_btc to gdax_usd
  Reachable from gdax_usd to gdax_btc
combineRWSTSpec
  run valid updateRates, it shouldn't proceed to findBestRate
  invalid updateRates, it should proceed to findBestRate
  both updateRates and findBestRate are invalid, it should display all eror message
findBestRateSpec
  failed due to source vertex not exists
  failed due to dest vertex not exists
  no matter orig AppState is InSync or OutSync, it will show the same path and return InSync
updateRatesSpec
  orig AppState has empty UserInput, success update should return OutSync with added rates
  no matter orig AppState is either InSync or OutSync, success update should return OutSync of added rates
  update the rate by the newer timestamp no matter the exchange currency cases
  no matter orig AppState is either InSync or OutSync, AppState remain the same as input ts are not newer

Finished in 0.0178 seconds
37 examples, 0 failures
```

## Appendix
Sample input, besides displaying the required information for the best rate and the vertices involved, it will display error messages of invalid input, latest exchange rates to make sure the application run correctly.
```
stack exec fsm-rwst-parsec-floydWarshall-exe 
2017-11-01T09:42:23+00:00 KRAKEN BTC USD 1000.0 0.434d
System unexpecting: "d"
Expecting: digit
General error: Product of 1000.0 and 0.434 must be <= 1.0
Invalid request to update rates, probably a request for best rate
System unexpecting: "2"
System unexpecting: "2"
Expecting: space
Expecting: letter
You neither enter exchange rates or request best rate, please enter a valid input
2017-11-01T09:42:23+00:00 KRAKEN BTC USD -1 0.0
System unexpecting: "-"
System unexpecting: "-"
Expecting: space
Expecting: digit
Invalid request to update rates, probably a request for best rate
System unexpecting: "2"
System unexpecting: "2"
Expecting: space
Expecting: letter
You neither enter exchange rates or request best rate, please enter a valid input
2017-11-01T09:42:23+00:00 KRAKEN BTC USD 1000.0 1.1
System unexpecting: 
Expecting: digit
General error: Product of 1000.0 and 1.1 must be <= 1.0
Invalid request to update rates, probably a request for best rate
System unexpecting: "2"
System unexpecting: "2"
Expecting: space
Expecting: letter
You neither enter exchange rates or request best rate, please enter a valid input
2017-11-01T09:42:23+00:00 KRAKEN BTC USD 1000.0 0.0009
(KRAKEN, BTC) -- 1000.0 2017-11-01 09:42:23 UTC --> (KRAKEN, USD)
(KRAKEN, USD) -- 9.0e-4 2017-11-01 09:42:23 UTC --> (KRAKEN, BTC)
KRAKEN BTC KRAKEN BTC
System unexpecting: " "
General error: Invalid timestamp: KRAKEN
Invalid request to update rates, probably a request for best rate
System unexpecting: 
Expecting: letter
General error: source must be different from destination
You neither enter exchange rates or request best rate, please enter a valid input
KRAKEN BTC GDAX BTC
System unexpecting: " "
General error: Invalid timestamp: KRAKEN
Invalid request to update rates, probably a request for best rate
(GDAX, BTC) is not entered before
You neither enter exchange rates or request best rate, please enter a valid input
KRAKEN BTC GDAX USD
System unexpecting: " "
General error: Invalid timestamp: KRAKEN
Invalid request to update rates, probably a request for best rate
(GDAX, USD) is not entered before
You neither enter exchange rates or request best rate, please enter a valid input
KRAKEN BTC KRAKEN USD
System unexpecting: " "
General error: Invalid timestamp: KRAKEN
Invalid request to update rates, probably a request for best rate
BEST_RATES_BEGIN KRAKEN BTC KRAKEN USD 1000.0
KRAKEN, BTC
KRAKEN, USD
BEST_RATES_END
KRAKEN USD KRAKEN BTC
System unexpecting: " "
General error: Invalid timestamp: KRAKEN
Invalid request to update rates, probably a request for best rate
BEST_RATES_BEGIN KRAKEN USD KRAKEN BTC 9.0e-4
KRAKEN, USD
KRAKEN, BTC
BEST_RATES_END
2017-11-01T09:43:23+00:00 GDAX BTC USD 1001.0 0.0008
(GDAX, BTC) -- 1001.0 2017-11-01 09:43:23 UTC --> (GDAX, USD)
(GDAX, USD) -- 8.0e-4 2017-11-01 09:43:23 UTC --> (GDAX, BTC)
(KRAKEN, BTC) -- 1000.0 2017-11-01 09:42:23 UTC --> (KRAKEN, USD)
(KRAKEN, USD) -- 9.0e-4 2017-11-01 09:42:23 UTC --> (KRAKEN, BTC)
KRAKEN BTC GDAX BTC
System unexpecting: " "
General error: Invalid timestamp: KRAKEN
Invalid request to update rates, probably a request for best rate
BEST_RATES_BEGIN KRAKEN BTC GDAX BTC 1.0
KRAKEN, BTC
GDAX, BTC
BEST_RATES_END
KRAKEN BTC GDAX USD
System unexpecting: " "
General error: Invalid timestamp: KRAKEN
Invalid request to update rates, probably a request for best rate
BEST_RATES_BEGIN KRAKEN BTC GDAX USD 1001.0
KRAKEN, BTC
GDAX, BTC
GDAX, USD
BEST_RATES_END
GDAX USD KRAKEN BTC
System unexpecting: " "
General error: Invalid timestamp: GDAX
Invalid request to update rates, probably a request for best rate
BEST_RATES_BEGIN GDAX USD KRAKEN BTC 9.0e-4
GDAX, USD
KRAKEN, USD
KRAKEN, BTC
BEST_RATES_END
```

## Reference

* https://www.youtube.com/watch?v=oNI0rf2P9gE&t=604s explains Floyd-Warshall algorithm and why it's preferred to Dijkstra's algorithm.
* https://wiki.haskell.org/Simple_RWST_use RWST explained
* http://hackage.haskell.org/package/writer-cps-mtl-0.1.1.5/docs/Control-Monad-RWS-CPS.html other RWST helping functions
* http://jakewheat.github.io/intro_to_parsing/ introduces Haskell Parsec
* https://www.schoolofhaskell.com/school/to-infinity-and-beyond/pick-of-the-week/parsing-floats-with-parsec how to parse floating point value
