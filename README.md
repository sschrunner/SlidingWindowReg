# SlidingWindowReg package
<!-- badges: start -->
[![R-CMD-check](https://github.com/sschrunner/SlidingWindowReg/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/sschrunner/SlidingWindowReg/actions/workflows/R-CMD-check.yaml)
  <!-- badges: end -->

## A Sliding Multi-Window Regression Model

This R package presents a Sliding Multi-Window Regression model for hydrological inference. Given an input time series $x_t$ (describing rainfall in a hydrological setting) and a time-dependent target variable $y_t$ describing the gauged watershed runoff, the model is based on a set of $k$ lagged window kernels modeling one component of the overall water runoff each. For instance, surface flow, characterized by short time lags, may be represented by window 1, while groundwater flow is described by a second window with longer time lag.

Each window $i\in \{1,\dots,k\}$ is characterized by 
* a location parameter $\delta_i$ indicating the time lag, i.e. the distance between the window center and the estimated time point on the time axis, and
* a size parameter $\sigma_i$ defining the width $l_i$ of the window.
The predicted runoff associated with window $i$ is computed as the rolling average of the input time series $x_t$ with the window kernel parameters $\kappa_i$ as weights, i.e. $\kappa_i^T x_{S_i}$, where $S_i = \{t-\delta_i - l_i,\dots,t-\delta_i + l_i\}$ is the set of time lags $\delta_i - l_i$ to $\delta_i + l_i$, and $x_{S_i} = (x_{t-1},\dots,x_{t-l})$ denotes the vector of lagged observations.

The predicted output $y_t$ is modeled as a multiple linear regression model 

$$ y_t = \beta_1 (\kappa_1^T x_t) + \beta_2 (\kappa_2^T x_t) + \dots + \beta_k (\kappa_k^T x_t) + \varepsilon_t, $$

where $\varepsilon_t$ denotes the model error.

## Package structure

The core functionality of the package consists of a <tt>train</tt> and a <tt>predict</tt> function, respectively. Further, functions for evaluating and plotting model results and parameters are provided.

## Installation

This version of the R package can be installed as follows:

      remotes::install_github("sschrunner/SlidingWindowReg", build_manual = TRUE, build_vignettes = TRUE)

## Dependencies

- R (>= 3.5.0)
- combinat,
- dplyr,
- ggplot2,
- hydroGOF,
- knitr,
- methods,
- nloptr,
- parallel,
- pbapply,
- rdist,
- stats

## Contact

The implemented method was developed in collaboration between 

- Norwegian University of Life Sciences (NMBU), Ås, Norway, 
- University of British Columbia (UBC), Vancouver, Canada, and
- Simon Fraser University (SFU), Burnaby, Canada

This package is currently under development. For issues, feel free to contact [Stefan Schrunner](mailto:stefan.schrunner@nmbu.no).
