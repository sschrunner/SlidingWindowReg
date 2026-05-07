# Train model

Trains an `SWR` model based on input and target time series data.

## Usage

``` r
trainSWR(
  ts_input,
  ts_output,
  iter = 5,
  runs,
  log = FALSE,
  parallel,
  return = "best",
  param_selection = "best_bic",
  algorithm = "GENOUD"
)
```

## Arguments

- ts_input:

  a vector or ts object containing the input time series

- ts_output:

  a vector or ts object (on the same time scale as ts_input) containing
  the target time series

- iter:

  number of iterations (maximum number of windows)

- runs:

  **\[deprecated\]** number of independent model runs; no longer
  supported due to deterministic window initialization

- log:

  whether a log-linear model should be used

- parallel:

  **\[deprecated\]** should the runs be computed in parallel? If FALSE,
  all runs are computed in serial. If TRUE, all runs are computed in
  parallel with a maximum number of cores. If a scalar is provided, the
  number of cores is set manually. No longer supported for single-run
  models

- return:

  **\[deprecated\]** either "best" (best model run is returned), or
  "all" (all model runs are returned)

- param_selection:

  either "max" (maximum number of windows), or "best_rmse", "best_aic",
  or "best_bic" to optimize RMSE, AIC, or BIC, respectively

- algorithm:

  either "GENOUD" (genetic optimization using derivatives), or "BOBYQA"
  (bound optimization by quadratic approximation)

## Value

an object of type `SWR` model

## Details

The training procedure implements an iterative algorithm described in
(Schrunner et al. 2023) . A new window is added in each iteration, hence
the number of windows equals to the iteration counter. Input and output
time series are provided in `ts_input` and `ts_output`, respectively.
Both are required to have equal lengths. The optimization is performed
using the GENOUD algorithm (Mebane Jr. and Sekhon 2011) , implemented in
`rgenoud` (Mebane Jr. and Sekhon 2023) , or alternatively using the
BOBYQA algorithm (Powell 2009) , implemented in `nloptr` (Johnson 2021)
. As training hyperparameter, `iter` indicates the number of iterations,
which equals to the maximum number of windows selected by the model.
Parameters `return` and `param_selection` indicate which criterion
should be used to determine the number of windows; option are:
`return="all"` (no hyperparameter selection) or `return="best"`, which
allows one of the following options:

- `param_selection="best_aic"`: select model with lowest AIC,

- `param_selection="best_bic"`: select model with lowest BIC,

- `param_selection="best_rmse"`: select model with lowest RMSE.
  Arguments `runs`, `parallel` are deprecated and should not be used.

## References

Johnson SG (2021). *The NLopt nonlinear-optimization package*. v2.7.1,
<http://github.com/stevengj/nlopt>.  
  
Mebane Jr. W, Sekhon JS (2011). “Genetic Optimization Using Derivatives:
The rgenoud package for R.” *Journal of Statistical Software, 42(11):
1-26.*, **42(11)**.  
  
Mebane Jr. W, Sekhon JS (2023). *R Version of GENetic Optimization Using
Derivatives*. v5.9-0.10, <https://github.com/JasjeetSekhon/rgenoud>.  
  
Powell MJ (2009). “The BOBYQA algorithm for bound constrained
optimization without derivatives.” *Cambridge NA Report NA2009/06,
University of Cambridge, Cambridge*, **26**.  
  
Schrunner S, Janssen J, Jenul A, Cao J, Ameli AA, Welch WJ (2023). “A
Gaussian Sliding Windows Regression Model for Hydrological Inference.”
arXiv.org (preprint). 2306.00453.

## Examples

``` r
# train a model based on one year of observations
set.seed(42)
data(sampleWatershed)
mod <- trainSWR(sampleWatershed$rain[1:365],
                sampleWatershed$gauge[1:365],
                iter = 2)
#> Warning: The `runs` argument of `trainSWR()` is deprecated as of SlidingWindowReg 0.1.1.
#> ℹ Multiple model runs are not useful with deterministic window initializations.
#> Warning: The `parallel` argument of `trainSWR()` is deprecated as of SlidingWindowReg
#> 0.1.1.
#> ℹ Parallel model runs are not useful with deterministic window initializations.
#> Warning: The `return` argument of `trainSWR()` is deprecated as of SlidingWindowReg
#> 0.1.1.
#> ℹ Return argument is unused.
summary(mod)
#> SlidingWindowReg (SWR) model object with k = 2 windows
#> 
#> | window| delta| sigma| beta|
#> |------:|-----:|-----:|----:|
#> |      1|  0.17| 21.28| 0.36|
#> |      2|  0.86|  0.36| 0.67|
#> 
```
