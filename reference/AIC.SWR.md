# Information Criteria

Computes Akaike's Information Criterion (AIC) (Akaike 1974) and Bayesian
Information Criterion (BIC) (Schwarz 1978) .

## Usage

``` r
# S3 method for class 'SWR'
AIC(object, ts_input, ts_output, ...)

# S3 method for class 'SWR'
BIC(object, ts_input, ts_output, ...)
```

## Arguments

- object:

  an `SWR` model object created using
  [trainSWR](https://sschrunner.github.io/SlidingWindowReg/reference/trainSWR.md)

- ts_input:

  a vector or ts object containing the input time series

- ts_output:

  a vector or ts object (on the same time scale as ts_input) containing
  the target time series

- ...:

  currently unused

## Value

AIC or BIC value

## Functions

- `AIC(SWR)`: Akaike's Information Criterion

- `BIC(SWR)`: Bayesian Information Criterion

## References

Akaike H (1974). “A new look at the statistical model identification.”
*IEEE Transactions on Automatic Control*, **19**(6), 716–723.
[doi:10.1109/tac.1974.1100705](https://doi.org/10.1109/tac.1974.1100705)
.  
  
Schwarz G (1978). “Estimating the Dimension of a Model.” *The Annals of
Statistics*, **6**(2).
[doi:10.1214/aos/1176344136](https://doi.org/10.1214/aos/1176344136) .
