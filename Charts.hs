{-# LANGUAGE FlexibleContexts #-}

module Main where

import Data.Char (isSpace)
import BenchGraph (bgraph, defaultConfig, Config(..), ComparisonStyle(..))
import WithCli (withCli)
import Data.List

-------------------------------------------------------------------------------

benchmarks :: [String]
benchmarks =
    [ "streamly/async"
    , "streamly/ahead"
    , "streamly/parallel"
    , "async/mapConcurrently_"
    ]

createCharts :: String -> IO ()
createCharts input = do
    let cfg title = defaultConfig
            { chartTitle = Just title
            , outputDir = "charts"
            , comparisonStyle = CompareFull
            , classifyBenchmark = \bm ->
                case bm `elem` benchmarks of
                    True -> Just ("Operations", bm)
                    False -> Nothing
            , sortBenchmarks = \bs ->
                    let i = intersect benchmarks bs
                    in i ++ (bs \\ i)
            }

    -- links in README.rst eat up the space so we match the same
    let toOutfile title field =
               (filter (not . isSpace) (takeWhile (/= '(') title))
            ++ "-"
            ++ field

        makeOneGraph infile field title = do
            let title' =
                       title
                    ++ " (" ++ field ++ ")"
                    ++ " (Lower is Better)"
            bgraph infile (toOutfile title field) field (cfg title')

    putStrLn "Creating time charts..."
    makeOneGraph input "time" "Concurrency overhead (100,000 threads)"
    putStrLn "\nCreating allocation charts..."
    makeOneGraph input "allocated" "Concurrency overhead (100,000 threads)"
    putStrLn "\nCreating maxrss charts..."
    makeOneGraph input "maxrss" "Concurrency overhead (100,000 threads)"

-- Pass <input file>
main :: IO ()
main = withCli createCharts
