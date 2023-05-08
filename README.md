# SlidingWindowReg package
<!-- badges: start -->
[![R-CMD-check](https://github.com/sschrunner/SlidingWindowReg/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/sschrunner/SlidingWindowReg/actions/workflows/R-CMD-check.yaml)
  <!-- badges: end -->

## A Sliding Multi-Window Regression Model

This R package presents a Sliding Multi-Window Regression model for hydrological inference. Given an input time series $x_t$ (describing rainfall in a hydrological setting) and a time-dependent target variable $y_t$ describing the gauged watershed runoff, the model is based on a set of $k$ lagged window kernels modeling one component of the overall water runoff each. For instance, surface flow, characterized by short time lags, may be represented by window 1, while groundwater flow is described by a second window with longer time lag.

Each window is characterized by 
* a location parameter $\delta$ indicating the time lag, i.e. the distance between the window center and the estimated time point on the time axis, and
* a size parameter $\sigma$ indicating the width of the window.
An predicted runoff associated with window $i$ is given by computing the rolling average of the input time series $x_t$ with the window kernel parameters $\kappa_i$ as weights

$$ \kappa_i^T x_t. $$
