# help functions to encode parameter set as vector
encodeParam <- function(mix, param){
  return(
    c(mix, as.vector(param))
  )
}

# help function to decode parameter set from vector
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

# performs continuous optimization with respect to mixing and window parameters
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

# train an elementary model
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
         lambda = lambda,
         train_hist = train_hist,
         train_metrics = train_hist[nrow(train_hist),-1])
    )
}

#' @title Train model
#' @description combines both training steps
#' @param ts_input a vector or ts object containing the input time series
#' @param ts_output a vector or ts object (on the same time scale as ts_input) containing the target time series
#' @param iter number of iterations (maximum number of windows)
#' @param cv_fold number of folds for cross-validation
#' @param runs number of independent model runs
#' @param lambda a non-negative scalar or vector indicating the L1 regularization parameter(s)
#' @param log whether a log-linear model should be used
#' @param parallel should the runs be computed in parallel?
#' @examples
#' set.seed(42)
#' train(sampleWatershed$rain, sampleWatershed$gauge, iter = 2, cv_fold = 1, runs = 1, parallel = FALSE)
#' @import pbapply
#' @import parallel
#' @export
train <- function(ts_input, ts_output, iter = 10, cv_fold = 5, runs = 10, lambda = 0.1, log = FALSE, parallel = TRUE){

  if(length(ts_input) != length(ts_output)){
    stop("Error: input and output must be vectors of the same lengths")
  }

  # split by years for cross-validation and create folds
  years <- ceiling(length(ts_input) / 365)
  if(years < cv_fold){
    warning("Number of years is smaller than cv_fold parameter; setting cv_fold to 1.")
    cv_fold = 1
  }
  cutoff_years <- floor(seq(1, years + 1, length.out = cv_fold + 1))
  fold_assignment <- rep(1 : cv_fold, 365 * diff(cutoff_years))[1 : length(ts_input)]
  folds <- sapply(cv_fold : 1, setdiff, x = 1 : cv_fold, simplify = FALSE)

  # create list of runs to perform
  init_list <- expand.grid(lambda = lambda, fold = 1 : cv_fold, run = 1 : runs)[,c(1,2)]

  # train models
  if(parallel){
    # initialize cluster
    n_cores = parallel::detectCores()
    cl <- parallel::makeCluster(n_cores - 1)
    #clusterExport(cl, list("ts_input", "ts_output", "log", "iter", "lambda"), envir = environment())
    clusterExport(cl, list("ts_input", "ts_output", "log", "iter"), envir = environment())
    clusterExport(cl, list("folds", "fold_assignment"), envir = environment())

    # target function
    eval_fct <- function(x){
      # obtain training indices
      train_inds <- fold_assignment %in% folds[[ x["fold"] ]]
      # train model
      res <- train_inc(ts_input = ts_input[train_inds],
                       ts_output = ts_output[train_inds],
                       iter = iter,
                       lambda = x["lambda"],
                       log = log)
      # compute test metrics
      test_metrics <- eval_all(predict(ts_input[-train_inds],
                                       mix = res$mix,
                                       param = res$param,
                                       log = log),
                               ts_output[-train_inds])
      res$test_metrics <- test_metrics
      return(res)
    }

    # run computation
    #res <- pbapply::pbreplicate(runs, train_inc(ts_input, ts_output, iter, lambda, log), cl = cl)
    res <- pbapply::pbapply(init_list, 1,
                            eval_fct,
                            cl = cl)

    # close cluster
    parallel::stopCluster(cl)
  } else {
    # run computation
    #res <- pbapply::pbreplicate(runs, train_inc(ts_input, ts_output, iter, lambda, log))
    res <- pbapply::pbapply(init_list, 1,
                            eval_fct)
  }
  return(res)
}
