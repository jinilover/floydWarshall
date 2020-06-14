# Floyd–Warshall algorithm

## Application background
The user input the exchange rates between 2 currencies in an exchange.  The rate of the same currency between 2 exchanges is set to 1.0 by default.  Then a graph is formed by vertices presenting the exchange/currency pairs and the edges connecting the vertices are exchange rates.  That's the problem applicable by the Floyd–Warshall algorithm.  The goal is looking for the best rate between 2 currencies and trades involved between the exchange/currency pairs.

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
As mentioned before, rate of 1.0 is assigned for the same currency across different exchanges.  The following graph is formed.

![Alt text](https://github.com/jinilover/floydWarshall/blob/master/GraphOfExchanges.png)

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
* `Vertex` represents the exchange and currency.
* `RateEntry` contains the starting vertex, the list of vertices representing the trades involves from the starting vertex, the best rate between the starting vertex and the last vertex of the list.
* `Matrix` holds `RateEntry`s for the Floyd-Warshall algorithm.
* `AppState` which is either `InSync` or `OutSync`.  It applies the FSM concept.  `InSync` means the matrix is optimized with the latest cache `ExchRateTimes` that holds the user input exchange rates.  When the cache is updated with the rate update request, the matrix is no longer valid such that it should update `AppState` as `OutSync`.  In processing the best rate request, it uses the `AppState` to decide whether it should re-run the algorithm to optimize the matrix.  If it is `InSync`, it will not waste the time to re-run the algorithm.  Otherwise, it will re-run the algorithm and update `AppState` as `InSync`.

## Build application
* Under `floydWarshall` folder, to enter the nix shell, run `nix-shell`
* Under the nix shell, run `cabal build`

## Run application
* Under the nix shell, run `cabal run`
* Please refer to the "Appendix" section for details.

## Run unit-tests
* Under the nix shell, to build the tests, run `cabal new-build test:tests`
* To run the tests, run `cabal new-test test:tests`

```
Running 1 test suites...
Test suite tests: RUNNING...
Unit Tests
  Utils
    setToVector
      converts an empty Set:                                                                           OK (0.01s)
          ✓ converts an empty Set passed 100 tests.
      converts a non-empty Set:                                                                        OK
          ✓ converts a non-empty Set passed 100 tests.
  Parser
    parseRates
      invalid input
        invalid timestamp:                                                                             OK
            ✓ invalid timestamp passed 100 tests.
        rate product > 1:                                                                              OK
            ✓ rate product > 1 passed 100 tests.
        same currencies:                                                                               OK
            ✓ same currencies passed 100 tests.
        same currencies ignore case:                                                                   OK
            ✓ same currencies ignore case passed 100 tests.
        0 forward rate:                                                                                OK
            ✓ 0 forward rate passed 100 tests.
        -ve backward rate:                                                                             OK
            ✓ -ve backward rate passed 100 tests.
        invalid forward rate:                                                                          OK
            ✓ invalid forward rate passed 100 tests.
      valid input
        valid string:                                                                                  OK
            ✓ valid string passed 100 tests.
        valid string spaces ignored:                                                                   OK
            ✓ valid string spaces ignored passed 100 tests.
        valid string currency to upper case:                                                           OK
            ✓ valid string currency to upper case passed 100 tests.
    parseExchPair
      invalid input
        same source and destination:                                                                   OK
            ✓ same source and destination passed 100 tests.
      valid input
        valid string:                                                                                  OK
            ✓ valid string passed 100 tests.
        valid string spaces ignored:                                                                   OK
            ✓ valid string spaces ignored passed 100 tests.
        valid string ignore case:                                                                      OK
            ✓ valid string ignore case passed 100 tests.
  ProcessRequests
    serveReq
      invalid input
        invalid input for both updateRates and findBestRate:                                           OK
            ✓ invalid input for both updateRates and findBestRate passed 100 tests.
      valid input
        valid `updateRates` request, so not proceed to `findBestRate` request:                         OK
            ✓ valid `updateRates` request, so not proceed to `findBestRate` request passed 100 tests.
        invalid `updateRates`, proceed to `findBestRate`:                                              OK
            ✓ invalid `updateRates`, proceed to `findBestRate` passed 100 tests.
    updateRates
      update empty state with added rates:                                                             OK
          ✓ update empty state with added rates passed 100 tests.
      update always makes the state `OutSync`:                                                         OK
          ✓ update always makes the state `OutSync` passed 100 tests.
      only input having newer timestamp make the update:                                               OK
          ✓ only input having newer timestamp make the update passed 100 tests.
      not update if input doesn't have newer timestamp:                                                OK
          ✓ not update if input doesn't have newer timestamp passed 100 tests.
    findBestRate
      invalid input
        source vertex not exists:                                                                      OK
            ✓ source vertex not exists passed 100 tests.
        destination vertex not exists:                                                                 OK
            ✓ destination vertex not exists passed 100 tests.
        same `UserInput` always produces the same result no matter the state is `InSync` or `OutSync`: OK
            ✓ same `UserInput` always produces the same result no matter the state is `InSync` or `OutSync` passed 100 tests.
  Algorithms
    buildMatrix
      build empty matrix for empty vertex:                                                             OK
          ✓ build empty matrix for empty vertex passed 100 tests.
      build a 4*4 matrix:                                                                              OK
          ✓ build a 4*4 matrix passed 100 tests.
      build a 7*7 matrix:                                                                              OK
          ✓ build a 7*7 matrix passed 100 tests.
    floydWarshall
      handle empty matrix:                                                                             OK
          ✓ handle empty matrix passed 100 tests.
      handle 4*4 matrix:                                                                               OK
          ✓ handle 4*4 matrix passed 100 tests.
      handle 7*7 matrix:                                                                               OK
          ✓ handle 7*7 matrix passed 100 tests.
    optimum
      src or dest not exists in the graph:                                                             OK
          ✓ src or dest not exists in the graph passed 100 tests.
      handle if it's reachable or not between src and dest:                                            OK
          ✓ handle if it's reachable or not between src and dest passed 100 tests.

All 34 tests passed (0.04s)
Test suite tests: PASS
```

## Appendix
Sample input, besides displaying the required information for the best rate and the vertices involved, it will display error messages of invalid input, latest exchange rates to make sure the application run correctly.

```
cabal run
Preprocessing library for floydWarshall-0.1.0.0..
Building library for floydWarshall-0.1.0.0..
Preprocessing executable 'floydWarshall' for floydWarshall-0.1.0.0..
Building executable 'floydWarshall' for floydWarshall-0.1.0.0..
Running floydWarshall...
2017-11-01T09:42:23+00:00 KRAKEN BTC USD 1000.0 0.434d
Failed reading: Product of 1000.0 and 0.434 must be <= 1.0
Invalid request to update rates, probably a request for best rate
letter: Failed reading: satisfy
You neither enter exchange rates or request best rate, please enter a valid input

2017-11-01T09:42:23+00:00 KRAKEN BTC USD -1 0.0
Failed reading: Rate must be > 0
Invalid request to update rates, probably a request for best rate
letter: Failed reading: satisfy
You neither enter exchange rates or request best rate, please enter a valid input

2017-11-01T09:42:23+00:00 KRAKEN BTC USD 1000.0 1.1
Failed reading: Product of 1000.0 and 1.1 must be <= 1.0
Invalid request to update rates, probably a request for best rate
letter: Failed reading: satisfy
You neither enter exchange rates or request best rate, please enter a valid input

2017-11-01T09:42:23+00:00 KRAKEN BTC USD 1000.0 0.0009
(KRAKEN, BTC) -- 1000.0 2017-11-01 09:42:23 UTC --> (KRAKEN, USD)
(KRAKEN, USD) -- 9.0e-4 2017-11-01 09:42:23 UTC --> (KRAKEN, BTC)

KRAKEN BTC KRAKEN BTC
Failed reading: parseTimeM: no parse of "KRAKEN"
Invalid request to update rates, probably a request for best rate
Failed reading: source must be different from destination
You neither enter exchange rates or request best rate, please enter a valid input

KRAKEN BTC GDAX BTC
Failed reading: parseTimeM: no parse of "KRAKEN"
Invalid request to update rates, probably a request for best rate
(GDAX, BTC) is not entered before
You neither enter exchange rates or request best rate, please enter a valid input

KRAKEN BTC GDAX USD
Failed reading: parseTimeM: no parse of "KRAKEN"
Invalid request to update rates, probably a request for best rate
(GDAX, USD) is not entered before
You neither enter exchange rates or request best rate, please enter a valid input

KRAKEN BTC KRAKEN USD
BEST_RATES_BEGIN KRAKEN BTC KRAKEN USD 1000.0
(KRAKEN, BTC)
(KRAKEN, USD)
BEST_RATES_END

KRAKEN USD KRAKEN BTC
BEST_RATES_BEGIN KRAKEN USD KRAKEN BTC 9.0e-4
(KRAKEN, USD)
(KRAKEN, BTC)
BEST_RATES_END

2017-11-01T09:43:23+00:00 GDAX BTC USD 1001.0 0.0008
(GDAX, BTC) -- 1001.0 2017-11-01 09:43:23 UTC --> (GDAX, USD)
(GDAX, USD) -- 8.0e-4 2017-11-01 09:43:23 UTC --> (GDAX, BTC)
(KRAKEN, BTC) -- 1000.0 2017-11-01 09:42:23 UTC --> (KRAKEN, USD)
(KRAKEN, USD) -- 9.0e-4 2017-11-01 09:42:23 UTC --> (KRAKEN, BTC)

KRAKEN BTC GDAX BTC
BEST_RATES_BEGIN KRAKEN BTC GDAX BTC 1.0
(KRAKEN, BTC)
(GDAX, BTC)
BEST_RATES_END

KRAKEN BTC GDAX USD
BEST_RATES_BEGIN KRAKEN BTC GDAX USD 1001.0
(KRAKEN, BTC)
(GDAX, BTC)
(GDAX, USD)
BEST_RATES_END

GDAX USD KRAKEN BTC
BEST_RATES_BEGIN GDAX USD KRAKEN BTC 9.0e-4
(GDAX, USD)
(KRAKEN, USD)
(KRAKEN, BTC)
BEST_RATES_END
```

## Load the modules under `test`
Run `cabal repl test:tests`

## Reference

* https://www.youtube.com/watch?v=oNI0rf2P9gE&t=604s explains Floyd-Warshall algorithm and why it's preferred to Dijkstra's algorithm.
* https://github.com/jinilover/mtl-classy-prism explains what is mtl and the problem it solves.
* https://wiki.haskell.org/Simple_RWST_use RWST explained
* http://hackage.haskell.org/package/writer-cps-mtl-0.1.1.5/docs/Control-Monad-RWS-CPS.html other RWST helping functions