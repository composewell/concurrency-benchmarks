{-# LANGUAGE FlexibleContexts #-}

module Main where

import Data.Char (isSpace)
import BenchGraph (bgraph, defaultConfig, Config(..), ComparisonStyle(..))
import WithCli (withCli)
import Data.List

-------------------------------------------------------------------------------

benchmarks0ms :: [String]
benchmarks0ms =
    [ "delay-0ms-10k/streamly/async"
    , "delay-0ms-10k/streamly/ahead"
    , "delay-0ms-10k/async/mapConcurrently"
    ]

benchmarks5s :: [String]
benchmarks5s =
    [ "delay-5000ms-10k/streamly/async"
    , "delay-5000ms-10k/streamly/ahead"
    , "delay-5000ms-10k/async/mapConcurrently"
    ]

dropPrefix :: String -> String
dropPrefix = drop 1 . dropWhile (/= '/')

createCharts :: String -> [String] -> String -> IO ()
createCharts desc benchmarks input = do
    let cfg title = defaultConfig
            { chartTitle = Just title
            , outputDir = "charts"
            , comparisonStyle = CompareFull
            , classifyBenchmark = \bm ->
                case bm `elem` benchmarks of
                    True -> Just ("Operations", dropPrefix bm)
                    False -> Nothing
            , sortBenchmarks = \bs ->
                    let i = intersect (map dropPrefix benchmarks) bs
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

    let t = "10,000 tasks, " ++ desc
    putStrLn "Creating time charts..."
    makeOneGraph input "time" t
    putStrLn "\nCreating maxrss charts..."
    makeOneGraph input "maxrss" t

createAllCharts :: String -> IO ()
createAllCharts input = do
    createCharts "0 sec delay" benchmarks0ms input
    createCharts "5 sec delay" benchmarks5s input

-- Pass <input file>
main :: IO ()
main = withCli createAllCharts
