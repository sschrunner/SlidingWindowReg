#' @title Compute all available model metrics
#' @description computes all evaluation metrics given a prediction and a reference vector
#' @param prediction a vector or ts object of predicted values
#' @param reference a vector or ts object (on the same time scale as prediction) containing ground truth values
#' @seealso \link[hydroGOF]{NSE}
#' @seealso \link[hydroGOF]{KGE}
#' @export
eval_all <- function(prediction, reference){
  res <- data.frame(
    rmse = rmse(prediction, reference),
    psnr = psnr(prediction, reference),
    r2 = r2(prediction, reference),
    nse = nse(prediction, reference),
    kge = kge(prediction, reference))
  return(res)
}

#' @describeIn eval_all Root Mean Squared Error
#' @export
rmse <- function(prediction, reference){
  if(length(prediction) != length(reference)){
    stop("Error in RMSE: prediction and reference must be vectors of the same length")
  }
  diff <- na.omit(prediction - reference)
  return(sqrt(t(diff) %*% diff / length(diff)))
}

#' @describeIn eval_all Normalized Root Mean Squared Error
#' @export
nrmse <- function(prediction, reference){
  return(rmse(prediction, reference) / mean(reference, na.rm = TRUE))
}

#' @describeIn eval_all Coefficient of Determination
#' @export
r2 <- function(prediction, reference){
  if(length(prediction) != length(reference)){
    stop("Error in r2: prediction and reference must be vectors of the same length")
  }
  return(1 - sum((prediction - reference)^2, na.rm = TRUE) / sum((reference - mean(reference, na.rm = TRUE))^2, na.rm = TRUE))
}

#' @describeIn eval_all Nash-Sutcliffe Efficiency
#' @import hydroGOF
#' @export
nse <- function(prediction, reference){
  if(length(prediction) != length(reference)){
    stop("Error in NSE: prediction and reference must be vectors of the same length")
  }
  return(NSE(prediction, reference))
}

#' @describeIn eval_all Kling-Gupta Efficiency
#' @import hydroGOF
#' @export
kge <- function(prediction, reference){
  if(length(prediction) != length(reference)){
    stop("Error in KGE: prediction and reference must be vectors of the same length")
  }
  return(KGE(prediction, reference))
}
