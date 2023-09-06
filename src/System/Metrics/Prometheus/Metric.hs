module System.Metrics.Prometheus.Metric where

import System.Metrics.Prometheus.Metric.Counter (
    Counter,
    CounterSample,
 )
import System.Metrics.Prometheus.Metric.Gauge (Gauge, GaugeSample)
import System.Metrics.Prometheus.Metric.Histogram (
    Histogram,
    HistogramSample,
 )
import System.Metrics.Prometheus.Metric.Summary (Summary, SummarySample)


data Metric
    = CounterMetric Counter
    | GaugeMetric Gauge
    | SummaryMetric Summary
    | HistogramMetric Histogram


data MetricSample
    = CounterMetricSample CounterSample
    | GaugeMetricSample GaugeSample
    | HistogramMetricSample HistogramSample
    | SummaryMetricSample SummarySample


metricSample ::
    (CounterSample -> a) ->
    (GaugeSample -> a) ->
    (HistogramSample -> a) ->
    (SummarySample -> a) ->
    MetricSample ->
    a
metricSample f _ _ _ (CounterMetricSample s) = f s
metricSample _ f _ _ (GaugeMetricSample s) = f s
metricSample _ _ f _ (HistogramMetricSample s) = f s
metricSample _ _ _ f (SummaryMetricSample s) = f s
