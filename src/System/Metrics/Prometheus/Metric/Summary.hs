{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}

module System.Metrics.Prometheus.Metric.Summary
  (Observation
  ,Summary(..)
  ,Quantile
  ,SummarySample(..)
  ,new
  ,observe
  ,sample
   -- ,testSummary ,calcQuantiles
  ) where

import Data.Map.Strict (Map)
import Data.IORef (IORef, newIORef, atomicModifyIORef, atomicModifyIORef'
                  ,writeIORef)
import qualified Data.Map as Map
import Data.Scientific (Scientific)
import Data.List (sort, foldl')
import Data.Time.Clock.System (SystemTime(..), getSystemTime)
import Data.Int (Int64)
import qualified Debug.Trace as Debug

type Observation = (SystemTime, Double)
data Summary = Summary {sumObservations        :: IORef [Observation]
                       ,sumQuantilesConfigured :: [Scientific]
                       ,sumMaxAge              :: Int64 -- ^ In seconds
                       }

type Quantile = Scientific

data SummarySample = SummarySample
    { sumQuantiles  :: !(Map Scientific Double)
    , sumSum        :: !Double
    , sumCount      :: !Int
    }
  deriving Show

new :: [Quantile] -> Int64 -> IO Summary
new quantiles maxAge =
    Summary <$> newIORef [] <*> pure (sort quantiles) <*> pure maxAge


observe :: Double -> Summary -> IO ()
observe !v Summary{sumObservations=obsRef} = do
    now <- getSystemTime
    atomicModifyIORef' obsRef (update now)
  where
    update :: SystemTime -> [Observation] -> ([Observation], ())
    update time obs = ((time, v) : obs, ())

data SummaryFoldState = SFS {sfsSample  :: !SummarySample
                            ,sfsRemQuantiles :: ![Scientific]}
  deriving Show

sample :: Summary -> IO SummarySample
sample Summary{sumObservations, sumQuantilesConfigured, sumMaxAge} = do
    st@MkSystemTime{systemSeconds} <- getSystemTime
    let cutoff = st{systemSeconds=systemSeconds-sumMaxAge}
    atomicModifyIORef sumObservations $ \observations ->
      let filtObs = Debug.traceShowId $ filter ((>= cutoff) . fst) observations
       in (filtObs
          ,calcQuantiles sumQuantilesConfigured $ snd <$> filtObs)


calcQuantiles :: [Scientific] -> [Double] -> SummarySample
calcQuantiles quantilesConfigured observations =
  sfsSample $
    foldl' (step (length observations))
           (SFS (SummarySample Map.empty 0 0) quantilesConfigured)
           (zip [0..] (sort $ observations))
  where
    step :: Int -> SummaryFoldState -> (Int, Double) -> SummaryFoldState
    step _totalLen sfs@SFS{sfsSample, sfsRemQuantiles=[]} (i,obs)
      = sfs{sfsSample=sfsSample{sumSum=sumSum sfsSample + obs
                               ,sumCount=sumCount sfsSample + 1}}
    step totalLen sfs@SFS{sfsSample, sfsRemQuantiles=q:qs} (i, obs)
      ----  | Debug.traceShow (sfs, i, obs) False = undefined
      -- Ratio-taking here is rearraged to avoid Scientifc division issues
      | sci (i+1) >= q * sci totalLen && sci i < q * sci totalLen
      = SFS{sfsSample=sfsSample{sumQuantiles=Map.insert q obs (sumQuantiles sfsSample)
                               ,sumSum=sumSum sfsSample + obs
                               ,sumCount=sumCount sfsSample + 1}
           ,sfsRemQuantiles=qs}
      | otherwise
      = SFS{sfsSample=sfsSample{sumQuantiles=sumQuantiles sfsSample
                               ,sumSum=sumSum sfsSample + obs
                               ,sumCount=sumCount sfsSample + 1}
           ,sfsRemQuantiles=q:qs}

    sci :: Integral a => a -> Scientific
    sci = fromIntegral


testSummary :: [(Int64, Double)]
            -- ^ Observations. First elem is seconds field of SystemTime
            -> Summary
            -> IO SummarySample
testSummary observations summary@Summary{sumObservations} = do
  let observations' = [(MkSystemTime secs 0, obs)
                      | (secs, obs) <- observations]
  writeIORef sumObservations observations'
  sample summary
