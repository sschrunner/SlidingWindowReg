# Compute all available model metrics

computes all evaluation metrics given a prediction and a reference
vector. The functionality includes general regression metrics and
hydrological metrics (wrapper to (Mauricio Zambrano-Bigiarini 2020) ):

- Root Mean Square Error (RMSE)

- Normalized Root Mean Square Error (NRMSE)

- Coefficient of Determination (R2)

- Nash-Sutcliffe Efficiency (Nash and Sutcliffe 1970)

- Kling-Gupta Efficiency (Gupta et al. 2009)

## Usage

``` r
eval_all(prediction, reference)

rmse(prediction, reference)

nrmse(prediction, reference)

mape(prediction, reference)

r2(prediction, reference)

nse(prediction, reference)

kge(prediction, reference)
```

## Arguments

- prediction:

  a vector or ts object of predicted values

- reference:

  a vector or ts object (on the same time scale as prediction)
  containing ground truth values

## Functions

- `rmse()`: Root Mean Squared Error

- `nrmse()`: Normalized Root Mean Squared Error

- `mape()`: Mean Absolute Percentage Error

- `r2()`: Coefficient of Determination

- `nse()`: Nash-Sutcliffe Efficiency

- `kge()`: Kling-Gupta Efficiency

## References

Gupta HV, Kling H, Yilmaz KK, Martinez GF (2009). “Decomposition of the
mean squared error and NSE performance criteria: Implications for
improving hydrological modelling.” *Journal of Hydrology*, **377**(1-2),
80–91.
[doi:10.1016/j.jhydrol.2009.08.003](https://doi.org/10.1016/j.jhydrol.2009.08.003)
.  
  
Mauricio Zambrano-Bigiarini (2020). *hydroGOF: Goodness-of-fit functions
for comparison of simulated and observed hydrological time series*.
[doi:10.5281/zenodo.839854](https://doi.org/10.5281/zenodo.839854) . R
package version 0.4-0, <https://github.com/hzambran/hydroGOF>.  
  
Nash JE, Sutcliffe JV (1970). “River flow forecasting through conceptual
models part I — A discussion of principles.” *Journal of Hydrology*,
**10**(3), 282–290.
[doi:10.1016/0022-1694(70)90255-6](https://doi.org/10.1016/0022-1694%2870%2990255-6)
.

## See also

[hydroGOF](https://hzambran.github.io/hydroGOF/reference/hydroGOF-package.html)
