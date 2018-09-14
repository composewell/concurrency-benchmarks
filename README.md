# concurrency-benchmarks

[![Hackage](https://img.shields.io/hackage/v/concurrency-benchmarks.svg?style=flat)](https://hackage.haskell.org/package/concurrency-benchmarks)
[![Build Status](https://travis-ci.org/composewell/concurrency-benchmarks.svg?branch=master)](https://travis-ci.org/composewell/concurrency-benchmarks)
[![Windows Build status](https://ci.appveyor.com/api/projects/status/wqban615v9f21xqi?svg=true)](https://ci.appveyor.com/project/harendra-kumar/concurrency-benchmarks)

Benchmarks to compare the concurrency overhead of
concurrent [streamly](https://github.com/composewell/streamly) streams and the
[async](https://hackage.haskell.org/package/async) package. If you are familiar
with `async`, see [this quick
guide](https://github.com/composewell/streamly/blob/master/docs/Async.md) for
equivalent features of streamly. For more detailed introduction to streamly see
[this tutorial](https://hackage.haskell.org/package/streamly/docs/Streamly-Tutorial.html).

## How to run

Run the `run.sh` script to run the benchmarks and create the charts. You can
use `cabal new-bench` or `stack bench` to run the benchmarks. To generate
charts, run the benchmarks with `--csv-raw=results.csv` option and then run
`makecharts results.csv`. Charts are generated in the `charts` directory.

IMPORTANT NOTE: THE `maxrss` FIGURES REPORTED BY GAUGE WILL NOT BE CORRECT
UNLESS YOU RUN ONE BENCHMARK AT A TIME. This is because `maxrss` is tracked per
process.

## Methodology

A total of 10,000 tasks are run for each concurrency mechanism being compared.
Two independent experiments are performed:

1. In the first experiment, each task is just a noop i.e. it takes almost 0 time
   to execute.
2. In the second experiment, each task introduces a 5 second delay

The first case shows streamly's smart scheduling to automatically run the tasks
in less number of threads than the actual number of tasks.  When the tasks do
not block and have a very low latency, streamly may run multiple tasks per
thread.  Therefore streamly is much faster on this benchmark.

In the second case a 5 second delay is introduced to make sure that streamly
uses one thread per task which is similar to what `async` does and therefore a
fair comparison.  For the `async` package, `mapConcurrently` is used which can
be compared with streamly's `ahead` style stream.

For streamly this is the code that is benchmarked, by default streamly has a
limit on the buffer size and the number of threads, we set those limits to `-1`
which means there is no limit:

```haskell
    let work = (\i -> threadDelay 5000000 >> return i)
    in runStream
        $ aheadly
        $ maxBuffer (-1)
        $ maxThreads (-1)
        $ S.fromFoldableM $ map work [1..10000]
```

For `async` this is the code that is benchmarked:

```haskell
    let work = (\i -> threadDelay 5000000 >> return i)
    mapConcurrently work [1..10000]
```

## Results

These charts compare
[streamly-0.5.1](https://hackage.haskell.org/package/streamly) and
`async-2.2.1` on a MacBook Pro with a 2.2 GHz Intel Core i7 processor.

When compiling, `-threaded -with-rtsopts "-N"` GHC options were used to enable
the use of multiple processor cores in parallel.

For streamly, results for both `async` and `ahead` style streams are shown.

### Zero delay case

#### Peak Memory Consumed

<img src="https://github.com/composewell/concurrency-benchmarks/blob/master/charts/10,000tasks,0secdelay-maxrss.svg" alt="Comparison of maxrss" width="640"/>

#### Time Taken

<img src="https://github.com/composewell/concurrency-benchmarks/blob/master/charts/10,000tasks,0secdelay-time.svg" alt="Comparison of time" width="640"/>

### 5 second delay case

#### Peak Memory Consumed

<img src="https://github.com/composewell/concurrency-benchmarks/blob/master/charts/10,000tasks,5secdelay-maxrss.svg" alt="Comparison of maxrss" width="640"/>

#### Time Taken

Note, this time shows the overhead only and not the full time taken by the
benchmark. For example the actual time taken by the `async` benchmark is
`5.135` seconds, but since 5 second in this is the delay introduced by each
parallel task, we compute the overhead of concurrency by deducting the 5
seconds from the actual time taken, so the overhead is `135 ms` in case of
`async`.

<img src="https://github.com/composewell/concurrency-benchmarks/blob/master/charts/10,000tasks,5secdelay-time.svg" alt="Comparison of time" width="640"/>

## Feedback

Feedback is welcome. Please raise an issue, send a PR or send an email to the
author.
