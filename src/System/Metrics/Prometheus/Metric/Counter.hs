module System.Metrics.Prometheus.Metric.Counter
       ( Counter
       , CounterSample (..)
       , new
       , add
       , inc
       , sample
       , addAndSample
       , set
       ) where


import           Control.Applicative  ((<$>))
import           Data.Atomics.Counter (AtomicCounter, incrCounter, newCounter, writeCounter)


newtype Counter = Counter { unCounter :: AtomicCounter }
newtype CounterSample = CounterSample { unCounterSample :: Int }


new :: IO Counter
new = Counter <$> newCounter 0


addAndSample :: Int -> Counter -> IO CounterSample
addAndSample by | by >= 0   = fmap CounterSample . incrCounter by . unCounter
                | otherwise = error "must be >= 0"


add :: Int -> Counter -> IO ()
add by c = addAndSample by c >> pure ()


inc :: Counter -> IO ()
inc = add 1


sample :: Counter -> IO CounterSample
sample = addAndSample 0

set :: Int -> Counter -> IO ()
set i (Counter c) = writeCounter c i
