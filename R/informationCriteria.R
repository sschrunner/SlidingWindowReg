#' @title Information Criteria
#' @description Computes Akaike's Information Criterion (AIC) and Bayesian Information Criterion (BIC).
#' @describeIn AIC.SWR Akaike's Information Criterion
#' @inheritParams summary.SWR
#' @inheritParams trainSWR
#' @return AIC or BIC value
#' @importFrom stats AIC
#' @importFrom methods is
#' @export
AIC.SWR <- function(object, ts_input, ts_output, ...){

  if(!is(object, "SWR")){
    stop("Wrong class of object")
  }

  lik <- loglik(predict(object, newdata = ts_input),
                ts_output)
  return(-2 * lik + 2 * (length(object$mix) + length(object$param)))
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
  return(-2 * lik + log(length(ts_input)) * (length(object$mix) + length(object$param)))
}

#' @describeIn AIC.SWR log-likelihood function
#' @importFrom stats var
#' @noRd
loglik <- function(prediction, reference){
  resid <- reference - prediction
  sigmasq <- var(resid, na.rm = TRUE)
  return( - abs(sum(!is.na(resid))) * log(sqrt(2 * pi * sigmasq)) - sum((resid)^2, na.rm = TRUE) / (2 * sigmasq))
}
