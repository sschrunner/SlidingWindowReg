#' @title Information Criteria
#' @description Computes Akaike's Information Criterion (AIC) \insertCite{akaike1974aic}{SlidingWindowReg} and Bayesian Information Criterion (BIC) \insertCite{schwarz1978bic}{SlidingWindowReg}.
#' @describeIn AIC.SWR Akaike's Information Criterion
#' @inheritParams summary.SWR
#' @inheritParams trainSWR
#' @return AIC or BIC value
#' @references \insertAllCited{}
#' @importFrom stats AIC
#' @importFrom methods is
#' @export
AIC.SWR <- function(object, ts_input, ts_output, ...){

  if(!is(object, "SWR")){
    stop("Wrong class of object")
  }

  lik <- loglik(predict(object, newdata = ts_input),
                ts_output)
  return(-2 * lik + 6 * dim(object) ) # -2 * loglik + 2 k
}

#' @describeIn AIC.SWR Bayesian Information Criterion
#' @importFrom stats BIC
#' @importFrom methods is
#' @export
BIC.SWR <- function(object, ts_input, ts_output, ...){

  if(!is(object, "SWR")){
    stop("Wrong class of object")
  }

  lik <- loglik(predict(object, newdata = ts_input),
                ts_output)
  return(-2 * lik + log(length(ts_input)) * 3 * dim(object)) # -2 * loglik + k * log(n)
}

#' @describeIn AIC.SWR log-likelihood function
#' @importFrom stats var
#' @noRd
loglik <- function(prediction, reference){
  rss <- sum((reference - prediction)^2)
  n <- length(prediction)
  return(-n/2 * (log(2*pi) + log(rss/n) + 1))
}
