{-# LANGUAGE OverloadedStrings #-}

module System.Metrics.Prometheus.Encode.Text.Summary where



import Data.ByteString.Builder (Builder)
import Data.List (intersperse)
import qualified Data.Map as Map
import System.Metrics.Prometheus.MetricId (
    MetricId (..),
    addLabel,
 )
import System.Metrics.Prometheus.Encode.Text.MetricId (
    encodeDouble,
    encodeInt,
    encodeLabels,
    encodeName,
    newline,
    space,
 )

import System.Metrics.Prometheus.Metric.Summary (SummarySample(..), Quantile)
import Data.Scientific (formatScientific, FPFormat (Fixed))
import Data.Text (pack)

-- # HELP check_common_paths_time_seconds Time taken by checkCommonPaths
-- # TYPE check_common_paths_time_seconds summary
-- check_common_paths_time_seconds{quantile="0.5",} 0.008303473
-- check_common_paths_time_seconds{quantile="0.9",} 0.023649391
-- check_common_paths_time_seconds{quantile="0.99",} 0.037654839
-- check_common_paths_time_seconds{quantile="0.999",} 0.041481906
-- check_common_paths_time_seconds{quantile="1.0",} 0.043044464
-- check_common_paths_time_seconds_count 7212.0
-- check_common_paths_time_seconds_sum 87.88258631400019

encodeSummary :: MetricId -> SummarySample -> Builder
encodeSummary mid summary =
    encodeSummaryQuantiles mid summary <> newline
        <> n
        <> "_sum"
        <> ls
        <> space
        <> encodeDouble (sumSum summary)
        <> newline
        <> n
        <> "_count"
        <> ls
        <> space
        <> encodeInt (sumCount summary)
  where
    n = encodeName $ name mid
    ls = encodeLabels $ labels mid


encodeSummaryQuantiles :: MetricId -> SummarySample -> Builder
encodeSummaryQuantiles mid =
    mconcat . intersperse newline . map snd . Map.toList
        . Map.mapWithKey (encodeSummaryQuantile mid)
        . sumQuantiles


encodeSummaryQuantile :: MetricId -> Quantile -> Double -> Builder
encodeSummaryQuantile mid quantile count =
    encodeName (name mid) <> encodeLabels labels' <> space <> encodeDouble count
  where
    labels' = addLabel "quantile" (formatQuantile quantile) (labels mid)
    formatQuantile = pack . formatScientific Fixed Nothing
