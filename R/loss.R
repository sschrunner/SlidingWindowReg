#' @title Compute all metrics
#' @description computes all evaluation metrics given a prediction and a reference vector
#' @export
eval_all <- function(prediction, reference){
  res <- data.frame(rmse = rmse(prediction, reference),
                    psnr = psnr(prediction, reference),
                    r2 = r2(prediction, reference),
                    nse = nse(prediction, reference),
                    kge = kge(prediction, reference))
  return(res)
}

#' @title Root Mean Squared Error
#' @description computes the RMSE given a prediction and a reference vector
#' @export
rmse <- function(prediction, reference){
  if(length(prediction) != length(reference)){
    stop("Error in RMSE: prediction and reference must be vectors of the same length")
  }
  diff <- na.omit(prediction - reference)
  return(sqrt(t(diff) %*% diff / length(diff)))
}

#' @title Normalized Root Mean Squared Error
#' @description computes the RMSE given a prediction and a reference vector, normalized with mean value of reference
#' @export
nrmse <- function(prediction, reference){
  return(rmse(prediction, reference) / mean(reference, na.rm = TRUE))
}

#' @title Root Mean Squared Error Inter-Quartile Range
#' @description computes the RMSE given a prediction and a reference vector, normalized with IQR of reference
#' @export
rmseiqr <- function(prediction, reference){
  return(rmse(prediction, reference) / IQR(reference, na.rm = TRUE))
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

#' @title R Squared
#' @description computes the R2 (coefficient of determination) given a prediction and a reference vector
#' @export
r2 <- function(prediction, reference){
  if(length(prediction) != length(reference)){
    stop("Error in r2: prediction and reference must be vectors of the same length")
  }
  return(1-sum((prediction - reference)^2, na.rm = TRUE) / sum((reference - mean(reference, na.rm = TRUE))^2, na.rm = TRUE))
}

#' @title NSE
#' @description computes the Nash-Sutcliffe Efficiency given a prediction and a reference vector
#' @import hydroGOF
#' @export
nse <- function(prediction, reference){
  if(length(prediction) != length(reference)){
    stop("Error in NSE: prediction and reference must be vectors of the same length")
  }
  return(NSE(prediction, reference))
}

#' @title KGE
#' @description computes the Kling-Gupta Efficiency given a prediction and a reference vector
#' @import hydroGOF
#' @export
kge <- function(prediction, reference){
  if(length(prediction) != length(reference)){
    stop("Error in KGE: prediction and reference must be vectors of the same length")
  }
  return(KGE(prediction, reference))
}
