-- |
-- Module      : Main
-- Copyright   : (c) 2018 Harendra Kumar
--
-- License     : BSD3
-- Maintainer  : harendra.kumar@gmail.com

import Gauge
import Streamly
import System.Random (randomRIO)
import Control.Concurrent.Async

count :: Int
count = 100000

-------------------------------------------------------------------------------
-- Append
-------------------------------------------------------------------------------

{-# INLINE append #-}
append
    :: (Monoid (t IO Int), Monad (t IO))
    => (t IO Int -> SerialT IO Int) -> IO ()
append t = randomRIO (1,1) >>= \x -> runStream $ t $ foldMap return [x..count]

main :: IO ()
main = do
  defaultMain
    [
      bgroup "streamly"
      [ bench "serial"   $ nfIO $ append serially
      , bench "wSerial"  $ nfIO $ append wSerially
      , bench "ahead"    $ nfIO $ append aheadly
      , bench "async"    $ nfIO $ append asyncly
      , bench "wAsync"   $ nfIO $ append wAsyncly
      , bench "parallel" $ nfIO $ append parallely
      ]
      , bgroup "async"
      [ bench "mapConcurrently_" $ nfIO $ mapConcurrently_ return [1..count]
      , bench "mapConcurrently" $ nfIO $ mapConcurrently return [1..count]
      ]
   ]
