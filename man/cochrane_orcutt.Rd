% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/train.R
\name{cochrane_orcutt}
\alias{cochrane_orcutt}
\title{Cochrane-Orcutt procedure}
\usage{
cochrane_orcutt(model, ts_input, ts_output, ar = 1, ...)
}
\arguments{
\item{model}{an \code{SWR} model}

\item{ts_input}{a vector or ts object containing the input time series}

\item{ts_output}{a vector or ts object (on the same time scale as ts_input) containing the target time series}

\item{ar}{number of autoregressive lags}

\item{...}{parameters for re-training the model using \link{trainSWR}}
}
\description{
Cochrane-Orcutt procedure to resolve auto-correlated residuals in \code{SWR} models.
}
\details{
If an \code{SWR} model has auto-correlated residuals, the Cochrane-Orcutt procedure can be used to transform the data, such that auto-correlations are removed.
Afterwards, the \code{SWR} model is retrained on the transformed data. For details, see \insertCite{schrunner2023gaussian}{SlidingWindowReg}.
}
