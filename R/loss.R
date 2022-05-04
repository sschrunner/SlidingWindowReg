#' @title Root Mean Squared Error
#' @description computes the RMSE given a prediction and a reference vector
#' @export
rmse <- function(prediction, reference){
  if(length(prediction) != length(reference)){
    stop("Error in RMSE: prediction and reference must be vectors of the same length")
  }
  diff <- na.omit(prediction - reference)
  return(sqrt(t(diff) %*% diff))
}

#' @title L0 Error
#' @description computes the L0 loss given a prediction and a reference vector
#' @export
l0_norm <- function(prediction, reference){
  if(length(prediction) != length(reference)){
    stop("Error in l0-norm: prediction and reference must be vectors of the same length")
  }
  diff <- na.omit(prediction - reference)
  return(max(abs(diff)))
}

#' @title Peak Signal-to-Noise Error
#' @description computes the PSNR (peak signal-to-noise ratio) given a prediction and a reference vector
#' @export
psnr <- function(prediction, reference){
  if(length(prediction) != length(reference)){
    stop("Error in l0-norm: prediction and reference must be vectors of the same length")
  }
  return(-20 * log10(l0_norm(prediction, reference) / rmse(prediction, reference)))
}
