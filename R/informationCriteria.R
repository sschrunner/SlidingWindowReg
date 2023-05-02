#' @title Information Criteria
#' @description Computes Akaike's Information Criterion (AIC) and Bayesian Information Criterion (BIC).
#' @param prediction a vector or ts object of predicted values
#' @param reference a vector or ts object (on the same time scale as prediction) containing ground truth values
#' @param model a SlidingWindowReg model
#' @return AIC or BIC value
#' @describeIn AIC Akaike's Information Criterion
#' @export
AIC <- function(prediction, reference, model){
  lik <- loglik(prediction, reference)
  return(-2 * lik + 2 * (length(model$mix) + length(model$param)))
}

#' @describeIn AIC Bayesian Information Criterion
#' @export
BIC <- function(prediction, reference, model){
  lik <- loglik(prediction, reference)
  return(-2 * lik + log(length(reference)) * (length(model$mix) + length(model$param)))
}

#' @describeIn AIC log-likelihood function
#' @importFrom stats var
loglik <- function(prediction, reference){
  resid <- reference - prediction
  sigmasq <- var(resid, na.rm = TRUE)
  return( - abs(sum(!is.na(resid))) * log(sqrt(2 * pi * sigmasq)) - sum((resid)^2, na.rm = TRUE) / (2 * sigmasq))
}
