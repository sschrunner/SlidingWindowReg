#' help functions to encode parameter set as vector
encodeParam <- function(mix, param){
  return(
    c(mix, as.vector(param))
  )
}
#' help function to decode parameter set from vector
decodeParam <- function(x){
  num_win = (length(x) - 1) / 3
  mix = x[1 : (num_win + 1)]
  param = matrix(x[- (1 : (num_win + 1))],
                 ncol = 2,
                 nrow = num_win)
  return(
    list(
      mix = mix,
      param = param
    )
  )
}

#' performs continuous optimization with respect to mixing and window parameters
#' @import nloptr
train_both <- function(ts_input, ts_output, mix0, param0, lambda, log){

  # specify algorithm (conjugated gradient) and tolerance
  opts <- list("algorithm"="NLOPT_LN_BOBYQA",
               "xtol_rel"=1.0e-8)

  # perform optimization
  res <- nloptr(
    x0 = encodeParam(mix = mix0, param = param0),
    eval_f = function(x){
      param_list <- decodeParam(x)
      mix <- param_list$mix
      param <- param_list$param
      return(
        nrmse(
          predict(ts_input, mix, param, log),
                  ts_output) +
          lambda * sum(abs(mix))
        )},
    lb = rep(0, length(mix0) + length(param0)),
    opts = opts
  )$solution

  return(res)
}

#' train an elementary model
train_inc <- function(ts_input, ts_output, iter, lambda, log){

  # help function to compute model metrics to store training history
  append_hist <- function(train_hist, mix, param, operation_str){
    predict(ts_input, mix, param, log) -> p
    train_hist <- rbind(train_hist,
                        cbind(
                          operation = operation_str,
                          eval_all(p, ts_output)
                        )
    )
    return(train_hist)
  }
  train_hist <- c()

  # parameter initialization
  param <- matrix(,nrow = 0, ncol = 2)
  mix = 1

  # iteration counter
  i = 1

  # stop if (a) maximum number of iterations is reached, or (b) a window is cancelled out
  while(i <= iter & all(mix[-1] > 0)){

    # initialize and add new window parameters (delta = 0 for first window, random otherwise)
    new_param <- cbind(
      ifelse(i == 1, 0, sample(setdiff(0:255, param[,1]), size = 1)),
      sample(0:255, size = 1)
    )
    param <- rbind(param, new_param)
    mix <- c(mix, ifelse(i == 1, 1, 0))

    # set lambda to 0 for first window (no regularization)
    lambda0 <- ifelse(i == 1, 0, lambda)

    # train parameters
    p <- train_both(ts_input, ts_output, mix0 = mix, param0 = param, lambda = lambda0, log = log)

    # reshape and update parameters
    param_list <- decodeParam(p)
    mix <- param_list$mix
    param <- param_list$param

    # reorder windows by delta
    if(nrow(param) > 1){
      o <- order(param[,1])
      param <- param[o,]
      mix[-1] <- (mix[-1])[o]
    }

    # add history entry
    train_hist <- append_hist(train_hist, mix, param, paste("iteration", i))

    # increment counter
    i <- i+1
  }

  # remove cancelled windows (mix-parameter 0)
  rm_win <- which(mix[-1] == 0)
  if(length(rm_win) > 0){
    mix <- mix[- (rm_win + 1)]
    param <- param[- rm_win, , drop = FALSE]
  }

  return(
    list(mix = mix,
         param = param,
         hist = train_hist,
         metrics = train_hist[nrow(train_hist),])
    )
}

#' @title Train model
#' @description combines both training steps
#' @param ts_input a vector or ts object containing the input time series
#' @param ts_output a vector or ts object (on the same time scale as ts_input) containing the target time series
#' @param iter number of iterations (maximum number of windows)
#' @param runs number of independent model runs
#' @param lambda a non-negative scalar indicating the L1 regulatization parameter
#' @param log whether a log-linear model should be used
#' @param parallel should the runs be computed in parallel?
#' @examples
#' set.seed(42)
#' train(sampleWatershed$rain, sampleWatershed$gauge, iter = 2, runs = 2)
#' @import pbapply
#' @import parallel
#' @export
train <- function(ts_input, ts_output, iter = 10, runs = 10, lambda = NULL, log = FALSE, parallel = TRUE){

  if(is.null(lambda)){
    lambda = 0.1
  }

  if(parallel){
    # initialize cluster
    n_cores = parallel::detectCores()
    cl <- parallel::makeCluster(n_cores - 1)
    clusterExport(cl, list("ts_input", "ts_output", "log", "iter", "lambda"), envir = environment())

    # run computation
    res <- pbapply::pbreplicate(runs, train_inc(ts_input, ts_output, iter, lambda, log), cl = cl)

    # close cluster
    parallel::stopCluster(cl)
  } else {
    # run computation
    res <- pbapply::pbreplicate(runs, train_inc(ts_input, ts_output, iter, lambda, log))
  }
  return(res)
}
