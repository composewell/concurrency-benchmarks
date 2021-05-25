-- |
-- Module      : Main
-- Copyright   : (c) 2018 Harendra Kumar
--
-- License     : BSD3
-- Maintainer  : harendra.kumar@gmail.com

import Control.Concurrent
import Control.Concurrent.Async
import Control.Monad (when)
import System.Random (randomRIO)
import Streamly.Prelude (IsStream, SerialT)
import Data.IORef

import Gauge
import qualified Streamly.Prelude as Stream

-------------------------------------------------------------------------------
-- Append
-------------------------------------------------------------------------------

{-# INLINE append #-}
append :: IsStream t => Int -> Int -> (t IO Int -> SerialT IO Int) -> IO ()
append tcount d t = randomRIO (1,1) >>= \x ->
    let work = (\i -> when (d /= 0) (threadDelay d) >> return i)
    in Stream.drain
        $ t
        $ Stream.maxBuffer (-1)
        $ Stream.maxThreads (-1)
        $ Stream.fromFoldableM $ map work [x..tcount]

mkBgroup :: Int -> Int -> [Benchmark]
mkBgroup d n =
    let work = (\i -> when (d /= 0) (threadDelay d) >> return i)
    in [ bgroup "streamly"
        [ bench "ahead"    $ nfIO $ append n d Stream.fromAhead
        , bench "async"    $ nfIO $ append n d Stream.fromAsync
        -- , bench "wAsync"   $ nfIO $ append n d wAsyncly
        -- , bench "parallel" $ nfIO $ append n d parallely
        ]
        , bgroup "async"
        [ bench "mapConcurrently_" $ nfIO $ mapConcurrently_ work [1..n]
        , bench "mapConcurrently"  $ nfIO $ mapConcurrently work [1..n]
        ]
       ]

main :: IO ()
main = do
  defaultMain
    [ -- bgroup "delay-0ms-1k"    (mkBgroup 0     1000)
      bgroup "delay-0ms-10k"   (mkBgroup 0     10000)
      {-
    , bgroup "delay-0ms-100k"  (mkBgroup 0     100000)

    , bgroup "delay-1ms-1k"    (mkBgroup 1000   1000)
    , bgroup "delay-1ms-10k"   (mkBgroup 1000   10000)
    , bgroup "delay-1ms-100k"  (mkBgroup 1000   100000)

    , bgroup "delay-10ms-1k"   (mkBgroup 10000  1000)
    , bgroup "delay-10ms-10k"  (mkBgroup 10000  10000)
    , bgroup "delay-10ms-100k" (mkBgroup 10000  100000)

    , bgroup "delay-100ms-1k"   (mkBgroup 100000 1000)
    , bgroup "delay-100ms-10k"  (mkBgroup 100000 10000)
    , bgroup "delay-100ms-100k" (mkBgroup 100000 100000)

    , bgroup "delay-1000ms-1k"   (mkBgroup 1000000 1000)
    , bgroup "delay-1000ms-10k"  (mkBgroup 1000000 10000)
    , bgroup "delay-1000ms-100k" (mkBgroup 1000000 100000)

    , bgroup "delay-5000ms-1k"   (mkBgroup 5000000 1000)
    -}
    , bgroup "delay-5000ms-10k"  (mkBgroup 5000000 10000)
    -- , bgroup "delay-5000ms-100k" (mkBgroup 5000000 100000)
    , bgroup "forkIO-5000ms-10k" $
    let delay_ = threadDelay 5000000
        count = 10000 :: Int
        list = [1..count]
        work i = delay_ >> return i
    in
    [ bench "discard" $ nfIO $ do
        mapM_ (\i -> forkIO $ work i >> return ()) list
        threadDelay 6000000
    , bench "collect" $ nfIO $ do
        ref <- newIORef []
        mapM_ (\i -> forkIO $ work i >>=
               \j -> atomicModifyIORef ref $ \xs -> (j : xs, ())) list
        threadDelay 6000000
    ]
   ]
