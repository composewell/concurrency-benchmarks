# concurrency-benchmarks

[![Hackage](https://img.shields.io/hackage/v/concurrency-benchmarks.svg?style=flat)](https://hackage.haskell.org/package/concurrency-benchmarks)
[![Build Status](https://travis-ci.org/composewell/concurrency-benchmarks.svg?branch=master)](https://travis-ci.org/composewell/concurrency-benchmarks)
[![Windows Build status](https://ci.appveyor.com/api/projects/status/5u19xvm7sn7salrh?svg=true)](https://ci.appveyor.com/project/harendra-kumar/concurrency-benchmarks)

Benchmarks to compare the pure concurrency overhead of various flavors of
concurrent [streamly](https://github.com/composewell/streamly) streams and the
[async](https://hackage.haskell.org/package/async) package.

Use `cabal new-bench` or `stack bench` to run the benchmarks. To generate
charts, run the benchmarks with `--csv-raw=results.csv` option and then run
`makecharts results.csv`. Charts are generated in the `charts` directory.

## Methodology

A total of 100,000 tasks are run for each concurrency mechanism being compared.
Each task is a noop i.e. it does nothing, just returns. Therefore the benchmark
measures the pure overhead of concurrency for the tiniest possible tasks.

Streamly's `async` and `ahead` style streams may automatically run the tasks in
less number of threads than the actual number of tasks i.e. they may run
multiple tasks per thread, if the tasks do not block and have a very low
latency. Therefore these streams are much faster on this benchmark.  However,
the streamly `parallel` style stream guarantees that each task runs in a
separate thread however small it is. For the `async` package,
`mapConcurrently_` is used, which runs the tasks but does not collect the
results.

## Results

These charts compare the [streamly master
branch](https://github.com/composewell/streamly/commit/d73041c957d4211a6dc89624f0ebff54178bda6a)
and `async-2.2.1` on a MacBook Pro with a 2.2 GHz Intel Core i7 processor.

This chart shows the time taken for the benchmark completion.

[![Comparison of time](https://github.com/composewell/concurrency-benchmarks/blob/master/charts/Concurrencyoverhead-time.svg)](https://github.com/composewell/concurrency-benchmarks/blob/master/charts/Concurrencyoverhead-time.svg)

This chart shows the maximum rss (resident set size), in other words peak
memory consumed during the benchmark run.

[![Comparison of maxrss](https://github.com/composewell/concurrency-benchmarks/blob/master/charts/Concurrencyoverhead-maxrss.svg)](https://github.com/composewell/concurrency-benchmarks/blob/master/charts/Concurrencyoverhead-maxrss.svg)

## Feedback

Feedback is welcome. Please raise an issue, send a PR or send an email to the
author.
