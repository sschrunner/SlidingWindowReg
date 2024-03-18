# SlidingWindowReg package
<!-- badges: start -->
[![R-CMD-check](https://github.com/sschrunner/SlidingWindowReg/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/sschrunner/SlidingWindowReg/actions/workflows/R-CMD-check.yaml)
  <!-- badges: end -->

## A Gaussian Sliding Windows Regression Model

This R package presents a Sliding Windows Regression model with Gaussian kernels for hydrological inference, see [Schrunner et al. (2023)](https://arxiv.org/abs/2306.00453). Given an input time series $(x_t)\_{t \in T}$ (describing rainfall in a hydrological setting) and a time-dependent target variable $(y_t)\_{t \in T}$ describing the gauged watershed runoff, the model utilizes a set of $k$ lagged time windows to model one water path each. For instance, surface flow, characterized by short time lags, may be represented by the first window, while groundwater flow is described by a second window with longer time lags.

A lagged time window $W$ is an interval on the time axis bounded by time lags $s_{\min} < s_{\max}$ relative to the current time point $t$, 

$$W = [t-s_{\max},t-s_{\min}].$$ 

Instead of $s_{\min}$ and $s_{\max}$, $W$ is represented by the following parameters for simplicity:

* a location parameter $$\delta = \frac{s_{\min} + s_{\max}}{2}$$ indicating the window center on the time axis, i.e. the distance between the window center and the estimated time point on the time axis, and
* a size parameter $$\sigma = \lceil \frac{s_{\max}-s_{\min}}{6} \rceil$$ defining the width of the window.

Given such a window $W_i$, we define a Gaussian kernel $\kappa^{(i)}$ as a weight vector approximating the shape of a Gaussian probability density function $\varphi$ with mean $\mu=\delta_i$ and standard deviation $\sigma$. The predicted runoff associated with window $i$ is then computed as the convolution of the input time series $\left(x_t\right)\_{t\in T}$ with the window kernel $\kappa^{(i)}$ as weight vector, i.e. $\kappa^{(i)} \ast x_{W_i}$, where  $x_W = (x_{t-s_{\max}},\dots,x_{t-s_{\min}})$ denotes the vector of lagged observations and $\ast$ denotes the convolution operator.

Overall, the predicted output $y_t$ is modeled as a multiple linear regression model 

$$ y_t = \sum\limits_{i=1}^{k} \beta_i \left(\kappa^{(i)} \ast x_{W_i}\right) + \varepsilon_t, $$ 

where $\varepsilon_t$ denotes the model error. Due to practical considerations, we restrict regression parameters $\beta_1,\dots,\beta_k$ to non-negative numbers.

## Package structure

The core functionality of the package consists of functions <tt>train</tt> and <tt>predict</tt>, which perform the model fitting and forecasting steps, respectively. Further, functions for evaluating and plotting model results and parameters are provided.

The implementation builds on an S3 class <tt>SWR</tt>, which implements multiple generic functions, such as <tt>summary</tt>, <tt>plot</tt>, <tt>coef</tt>, <tt>dim</tt>, <tt>AIC</tt>, or <tt>BIC</tt>. Further, evaluation metrics can be computed using <tt>eval\_all</tt>, which calls regression performance metrics <tt>rmse</tt> (root mean square error, RMSE), <tt>nrmse</tt> (normalized RMSE), <tt>r2</tt> (coefficient of determination), as well as hydrological metrics <tt>nse</tt> (Nash-Sutcliffe efficiency) and <tt>kge</tt> (Kling-Gupta efficientcy). Plots are provided for the kernel vectors $\kappa^{(i)}$ via <tt>plot_kernel</tt>, as well as for the predictions using <tt>plot_prediction</tt>.

A sample dataset <tt>sampleWatershed</tt> is contained in the package. The data originates from a real-world watershed in Cowichan, British Columbia, Canada and contains daily precipitation (input time series), as well as gauged runoff (output time series). Both time series cover a period of 39 hydrological years, starting on October 01, 1979.

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
- lifecycle,
- methods,
- nloptr,
- rgenoud,
- parallel,
- pbapply,
- rdist,
- Rdpack (>= 0.7),
- stats

## Citation

If you use <tt>SlidingWindowReg</tt> in a report or scientific publication, we would appreciate citations to the following preprint:

Schrunner, S. et al. (2023). A Gaussian Sliding Windows Regression Model for Hydrological Inference. arXiv.org (preprint), 2023, https://doi.org/10.48550/arXiv.2306.00453

Bibtex entry:

	@misc{schrunner2023gaussian,
      title={A Gaussian Sliding Windows Regression Model for Hydrological Inference}, 
      author={Stefan Schrunner and Joseph Janssen and Anna Jenul and Jiguo Cao and Ali A. Ameli and William J. Welch},
      year={2023},
      howpublished={arXiv.org (preprint)},
      eprint={2306.00453},
      archivePrefix={arXiv},
      primaryClass={stat.ME},
      doi={10.48550/arXiv.2306.00453},
      url={https://doi.org/10.48550/arXiv.2306.00453}
      }

## Contact

The implemented Gaussian Sliding Windows Regression model was developed in collaboration between 

- Norwegian University of Life Sciences (NMBU), Ås, Norway, 
- University of British Columbia (UBC), Vancouver, Canada, and
- Simon Fraser University (SFU), Burnaby, Canada

This package is currently under development. For issues, feel free to contact [Stefan Schrunner](mailto:stefan.schrunner@nmbu.no).
