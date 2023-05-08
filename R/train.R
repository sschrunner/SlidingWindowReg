#' @describeIn train Encode parameter vector
#' @noRd
encodeParam <- function(mix, param){
  return(
    c(mix, as.vector(param))
  )
}

#' @describeIn train Decode parameter vector
#' @param x encoded parameter vector
#' @param intercept if TRUE, a model with intercept is used
#' @noRd
decodeParam <- function(x, intercept = FALSE){
  num_win = ifelse(intercept, (length(x) - 1), length(x)) / 3
  num_mix = ifelse(intercept, num_win + 1, num_win)
  mix = x[1 : num_mix]
  param = matrix(x[- (1 : num_mix)],
                 ncol = 2,
                 nrow = num_win)

  colnames(param) <- c("delta", "sigma")
  return(
    list(
      mix = mix,
      param = param
    )
  )
}

#' @describeIn train Evaluate model error
#' @noRd
error <- function(ts_input, ts_output, param, mix, log){
    return(rmse(predict(ts_input, mix, param, log), ts_output)
  )
}

#' @describeIn train Core training process
#' @import nloptr
#' @import dplyr
#' @noRd
train_inc <- function(ts_input, ts_output, iter, log, param_selection = "best_bic"){

  # help function to compute model metrics to store training history
  append_hist <- function(train_hist, mix, param, operation_str){
    predict(ts_input, mix, param, log) -> p
    train_hist <- rbind(train_hist,
                        cbind(
                          operation = operation_str,
                          aic = SlidingWindowReg::AIC(p, ts_output, list(mix = mix, param = param)),
                          bic = SlidingWindowReg::BIC(p, ts_output, list(mix = mix, param = param)),
                          eval_all(p, ts_output)
                        )
    )
    return(train_hist)
  }
  train_hist <- c()
  param_hist <- list()

  # parameter initialization
  param <- matrix(,nrow = 0, ncol = 2)
  mix = c() # VERSION WITH INTERCEPT: 1

  # iteration counter
  i = 1
  stop = FALSE

  while(!stop){
    if(i > 1){
      delta_new <- c(param[,1], max(param[,1] + 1))
      mix_new <- rep(sum(mix) / i, i)#VERSION WITH INTERCEPT: c(mix[1], rep(sum(mix[-1]) / i, i)) # use previous mixture and distribute upon i windows
      sigma_new <- rep(sum(param[,2]) / i, i) # use previous sigma and distribute upon i windows
    }
    else{
      delta_new <- 1
      mix_new <- 1 # VERSION WITH INTERCEPT: c(mean(ts_output),1)
      sigma_new <- 1
    }

    param <- cbind(delta_new, sigma_new)
    mix <- mix_new

    # RUN OPTIMIZATION ALGORITHM
    opts <- list("algorithm"="NLOPT_LN_BOBYQA",
                 "xtol_rel" = 0,
                 "ftol_rel" = 0,
                 "xtol_abs" = 0,
                 "ftol_abs" = 1e-8)

    # perform optimization
    res <- nloptr(
        x0 = encodeParam(mix = mix, param = param),
        eval_f = function(x){
         param_list <- decodeParam(x, intercept = FALSE)
         mix <- param_list$mix
         param <- param_list$param
         return(
           log(error(ts_input, ts_output, param, mix, log))
         )},
        lb = rep(0, length(mix) + length(param)),
        opts = opts
      )$solution %>%
      decodeParam(intercept = FALSE)
    mix <- res$mix
    param <- res$param

    # reorder windows by delta
    if(nrow(param) > 1){
      o <- order(param[,1])
      param <- param[o,]
      mix <- mix[o] #VERSION WITH INTERCEPT: mix[-1] <- (mix[-1])[o]
    }

    # add parameter history entry
    param_hist <- append(param_hist, list(list(mix = mix, param = param)))

    # add performance history entry
    train_hist <- append_hist(train_hist, mix, param, paste("iteration", i))

    # evaluate stopping criteria
    stop <- (stop | (i >= iter)) # stop if max iteration counter is reached

    # increment counter
    i <- i+1
  }

  ##### HYPERPARAMETER SELECTION #####
  if(param_selection == "best_rmse"){
    best_ind <- which.min(train_hist$rmse)
  }
  else if(param_selection == "best_aic"){
    best_ind <- which.min(train_hist$aic)
  }
  else if(param_selection == "best_bic"){
    best_ind <- which.min(train_hist$bic)
  }
  else if(param_selection == "max"){
    best_ind <- length(train_hist$rmse)
  }
  else{
    stop("param_selection method unknown!")
  }
  mix <- param_hist[[best_ind]]$mix
  param <- param_hist[[best_ind]]$param
  train_metrics <- train_hist[best_ind,-1]

  return(
    list(mix = mix,
         param = param,
         train_hist = train_hist,
         train_metrics = train_metrics,
         param_hist = param_hist)
  )
}

#' @title Train model
#' @description combines both training steps
#' @param ts_input a vector or ts object containing the input time series
#' @param ts_output a vector or ts object (on the same time scale as ts_input) containing the target time series
#' @param iter number of iterations (maximum number of windows)
#' @param runs number of independent model runs
#' @param log whether a log-linear model should be used
#' @param parallel should the runs be computed in parallel? If FALSE, all runs are computed in serial. If TRUE, all runs are computed in parallel with a maximum number of cores. If a scalar is provided, the number of cores is set manually.
#' @param return either "best" (best model run is returned), or "all" (all model runs are returned)
#' @param param_selection either "max" (maximum number of windows), or "best_rmse", "best_aic", or "best_bic" to optimize RMSE, AIC, or BIC, respectively
#' @examples
#' # train a model based on first year of observations
#' set.seed(42)
#' train(sampleWatershed$rain[1:365],
#'       sampleWatershed$gauge[1:365],
#'       iter = 2,
#'       runs = 1,
#'       parallel = FALSE)
#' @import pbapply
#' @import parallel
#' @export
train <- function(ts_input, ts_output, iter = 10, runs = 10, log = FALSE,
                  parallel = TRUE, return = "best", param_selection = "best_rmse"){

  if(length(ts_input) != length(ts_output)){
    stop("Error: input and output must be vectors of the same lengths")
  }

  if(!(return %in% c("best", "all"))){
    return = "best"
  }

  # create list of runs to perform
  init_list <- 1 : runs # parameter list

  # train models
  if(parallel == TRUE || is.numeric(parallel)){
    # initialize cluster
    if(is.numeric(parallel)){
      n_cores = min(parallel, parallel::detectCores() - 1)
    }
    else{
      n_cores = parallel::detectCores() - 1
    }
    cl <- parallel::makeCluster(n_cores)
    clusterExport(cl, list("ts_input", "ts_output", "log", "iter", "param_selection"), envir = environment())
    clusterExport(cl, list("error"), envir = environment())

    # run computation
    res <- pbapply::pbreplicate(runs,
                                train_inc(
                                  ts_input = ts_input,
                                  ts_output = ts_output,
                                  iter = iter,
                                  log = log,
                                  param_selection = param_selection),
                                simplify = FALSE,
                                cl = cl)

    # close cluster
    parallel::stopCluster(cl)
  } else {
    # run computation
    res <- pbapply::pbreplicate(runs,
                                train_inc(
                                  ts_input = ts_input,
                                  ts_output = ts_output,
                                  iter = iter,
                                  log = log,
                                  param_selection = param_selection),
                                simplify = FALSE)
  }

  # add fitted & residuals to list objects
  res <- lapply(1:length(res), function(x){
    r <- res[[x]]
    r$fitted <- predict(ts_input, r$mix, r$param)
    r$residuals <- ts_output - r$fitted
    return(r)})

  # reshape as array
  dim(res) <- runs
  names(res) <- paste("run",1:runs)

  if(return == "all"){
    return(res)
  }
  else{
    rmse <- sapply(res, function(x){return(x$train_metrics$rmse)})
    best_rmse <- which.min(rmse)
    res <- res[[best_rmse]]
    class(res) <- "SWR"
    return(res)
  }
}
