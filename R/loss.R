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
    nrmse = nrmse(prediction, reference),
    r2 = r2(prediction, reference),
    nse = nse(prediction, reference),
    kge = kge(prediction, reference))
  return(res)
}

#' @describeIn eval_all Root Mean Squared Error
#' @importFrom stats na.omit
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

#' @describeIn eval_all difference in number of windows
#' @export
diff_num_win <- function(model, reference_no){
  return(length(model$mix) - reference_no)
}

#' @describeIn kernel overlap
#' @export
overlap <- function(param_prediction, param_reference){
  k1 <- get_kernel(param_prediction, type = "combined") / nrow(param_prediction)
  k2 <- get_kernel(param_reference, type = "combined") / nrow(param_reference)

  len_diff <- length(k1) - length(k2)
  if(len_diff > 0){
    k2 <- c(rep(0, len_diff), k2)
  } else if(len_diff < 0){
    k1 <- c(rep(0, -len_diff), k1)
  }

  k <- rbind(k1, k2)

  return(sum(apply(k, 2, min)))
}

#' @describeIn MAD of window coefficients
#' @export
mad_window_coeff <- function(beta_prediction, beta_reference){
  n_pred <- length(beta_prediction)
  n_ref <- length(beta_reference)

  if(n_pred > n_ref){
    beta_reference <- c(beta_reference, rep(0, n_pred - n_ref))
  }
  else if(n_pred < n_ref){
    beta_prediction <- c(beta_prediction, rep(0, n_ref - n_pred))
  }

  return(sum(abs(beta_prediction - beta_reference)))
}

