# Cochrane-Orcutt procedure

Cochrane-Orcutt procedure to resolve auto-correlated residuals in `SWR`
models.

## Usage

``` r
cochrane_orcutt(model, ts_input, ts_output, ar = 1, ...)
```

## Arguments

- model:

  an `SWR` model

- ts_input:

  a vector or ts object containing the input time series

- ts_output:

  a vector or ts object (on the same time scale as ts_input) containing
  the target time series

- ar:

  number of autoregressive lags

- ...:

  parameters for re-training the model using
  [trainSWR](https://sschrunner.github.io/SlidingWindowReg/reference/trainSWR.md)

## Details

If an `SWR` model has auto-correlated residuals, the Cochrane-Orcutt
procedure can be used to transform the data, such that auto-correlations
are removed. Afterwards, the `SWR` model is retrained on the transformed
data. For details, see (Schrunner et al. 2023) .
