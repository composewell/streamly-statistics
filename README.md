# streamly-statistics

Statistical measures for finite or infinite data streams.

All operations use numerically stable floating point arithmetic. Measurements
can be performed over the entire input stream or on a sliding window of fixed
or variable size.  Where possible, measures are computed online without
buffering the input stream.

Includes:

* Summary: length, sum, powerSum
* Location: minimum, maximum, rawMoments, means, exponential smoothing
* Spread: range, variance, deviations
* Shape: skewness, kurtosis
* Sample statistics, resampling
* Probablity distribution: frequency, mode, histograms
* Transforms: Fast fourier transform
