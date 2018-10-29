# Application of Floyd-Warshall

## Background
https://www.youtube.com/watch?v=oNI0rf2P9gE&t=604s gave a good introduction and why it's preferred to Dijkstra's algorithm.

## Application areas
The video explained how to use Floyd-Warshall algorithm to find the shortest path.  However, we can also use it to find the path of the largest weight.  E.g. in a graph of different currency exchanges, we can use the algorithm to find the sequence of trades across exchanges to find the best exchange rate.

## Build application
* Under `apply-floydWarshall` folder, run `stack build`

## Run application
* Under `apply-floydWarshall` folder, run `stack exec apply-floydWarshall-exe`
* Please refer to the "Appendix" section for details.

## Run unit-tests
* Under `apply-floydWarshall` folder, run `stack build apply-floydWarshall\:test\:apply-floydWarshall-test`, it should run the tests as follows

```
setToMapVectorSpec
  converts an empty Set
  converts a Vertex Set to (Map Vertex Int, Vector Vectex) pair
parseExchRatesSpec
  failed due to invalid timestamp
  failed due to product of both rates bigger > 1
  failed due to both currencies are the same
  failed due to both currencies are the same even though they are different cases
  failed due to forward rate is 0
  failed due to backward rate is < 0
  failed due to forward rate is not numeric
  parse 2 valid strings
  update the rate by the newer timestamp
  convert exchange currency to upper case
parseRateReqSpec
  failed due to source vertex not exists
  failed due to source vertex not exists
  failed due to source and destination are the same
  parsed the source and destination
  parsed the source and destination in case insensitve manner
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

Finished in 0.0173 seconds
28 examples, 0 failures
```

## Appendix
Sample input, besides displaying the required information for the best rate and the vertices involved, it will display error messages of invalid input, latest exchange rates to make sure the application run correctly.
```
192-168-1-2:apply-floydWarshall amywong$ stack build
192-168-1-2:apply-floydWarshall amywong$ stack exec apply-floydWarshall-exe
2017-11-01T09:42:23+00:00 KRAKEN BTC USD 1000.0 0.434d
Invalid rate: 0.434d
2017-11-01T09:42:23+00:00 KRAKEN BTC USD -1 0.0
Rate must be > 0
2017-11-01T09:42:23+00:00 KRAKEN BTC USD 1000.0 1.1
Product of 1000.0 and 1.1 must be <= 1.0
2017-11-01T09:42:23+00:00 KRAKEN BTC USD 1000.0 0.0009
The most updated exchange rates:
(KRAKEN, BTC) -- 1000.0 2017-11-01 09:42:23 UTC --> (KRAKEN, USD)
(KRAKEN, USD) -- 9.0e-4 2017-11-01 09:42:23 UTC --> (KRAKEN, BTC)
KRAKEN BTC KRAKEN BTC
Source and destination must be different
KRAKEN BTC GDAX BTC
(GDAX, BTC) is not entered before
KRAKEN BTC GDAX USD
(GDAX, USD) is not entered before
KRAKEN BTC KRAKEN USD
BEST_RATES_BEGIN KRAKEN BTC KRAKEN USD 1000.0
KRAKEN, BTC
KRAKEN, USD
BEST_RATES_END
KRAKEN USD KRAKEN BTC
BEST_RATES_BEGIN KRAKEN USD KRAKEN BTC 9.0e-4
KRAKEN, USD
KRAKEN, BTC
BEST_RATES_END
2017-11-01T09:43:23+00:00 GDAX BTC USD 1001.0 0.0008
The most updated exchange rates:
(GDAX, BTC) -- 1001.0 2017-11-01 09:43:23 UTC --> (GDAX, USD)
(GDAX, USD) -- 8.0e-4 2017-11-01 09:43:23 UTC --> (GDAX, BTC)
(KRAKEN, BTC) -- 1000.0 2017-11-01 09:42:23 UTC --> (KRAKEN, USD)
(KRAKEN, USD) -- 9.0e-4 2017-11-01 09:42:23 UTC --> (KRAKEN, BTC)
KRAKEN BTC GDAX BTC
BEST_RATES_BEGIN KRAKEN BTC GDAX BTC 1.0
KRAKEN, BTC
GDAX, BTC
BEST_RATES_END
KRAKEN BTC GDAX USD
BEST_RATES_BEGIN KRAKEN BTC GDAX USD 1001.0
KRAKEN, BTC
GDAX, BTC
GDAX, USD
BEST_RATES_END
GDAX USD KRAKEN BTC
BEST_RATES_BEGIN GDAX USD KRAKEN BTC 9.0e-4
GDAX, USD
KRAKEN, USD
KRAKEN, BTC
BEST_RATES_END

Neither 6 input items for rate update nor 4 input items for best rate request, please re-enter
^C
```
