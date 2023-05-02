#' @describeIn overlap Computes the kernel overlap between each predicted window and each ground truth window separately
#' @return a vector of overlap values for each ground truth window
#' @importFrom rdist cdist
#' @importFrom combinat permn
#' @export
window_overlap <- function(param_prediction, param_reference){
  num_win <- nrow(param_reference)
  param_prediction <- rbind(param_prediction,
                            matrix(NA, nrow = max(num_win - nrow(param_prediction), 0), ncol = ncol(param_prediction)))

  pairwise_overlap_mat <- cdist(param_reference,
                                param_prediction,
                                metric = overlap)
  combinations <- combinat::permn(max(num_win,
                                      nrow(param_prediction))) %>%
    do.call(what = rbind) %>%
    as.data.frame() %>%
    select(1:num_win) %>%
    distinct()

  win_order <- combinations[which.max(apply(combinations, 1, function(x){
    return(sum(pairwise_overlap_mat[cbind(1:num_win, x)]))})),] %>%
    unlist()

  return(pairwise_overlap_mat[cbind(1:num_win, win_order)])
}

#' @title Kernel overlap
#' @description Computes the kernel overlap as evaluation metric between a predicted kernel and the ground truth kernel
#' @param param_prediction predicted window parameter matrix (2 columns)
#' @param param_reference ground truth window parameter matrix (2 columns)
#' @param mix_prediction vector of predicted regression parameters
#' @param mix_reference vector of ground truth regression parameters
#' @return a scalar indicating the overlap between predicted and ground truth kernel. Value between 0 (minimum overlap) and 1 (complete overlap).
#' @export
overlap <- function(param_prediction, param_reference, mix_prediction = NULL, mix_reference = NULL){
  if(is.vector(param_prediction)){
    param_prediction <- t(param_prediction)
  }
  if(is.vector(param_reference)){
    param_reference <- t(param_reference)
  }

  if(is.null(mix_prediction)){
    mix_prediction = rep(1 / nrow(param_prediction), nrow(param_prediction)) # VERSION WITH INTERCEPT: c(0,....)
  }
  if(is.null(mix_reference)){
    mix_reference = rep(1 / nrow(param_reference), nrow(param_reference)) # VERSION WITH INTERCEPT: c(0,....)
  }

  k1 <- get_kernel(param_prediction, type = "combined", mix = mix_prediction, weighted = TRUE)# / nrow(param_prediction)
  k2 <- get_kernel(param_reference, type = "combined", mix = mix_reference, weighted = TRUE)# / nrow(param_reference)

  len_diff <- length(k1) - length(k2)
  if(len_diff > 0){
    k2 <- c(rep(0, len_diff), k2)
  } else if(len_diff < 0){
    k1 <- c(rep(0, -len_diff), k1)
  }

  k <- rbind(k1, k2)

  return(sum(apply(k, 2, min)) / min(sum(mix_prediction), sum(mix_reference))) # VERSION WITH INTERCEPT: [-1]
}
