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
    return(-loglik(predict_target(ts_input, mix, param, log), ts_output)
  )
}

#' @describeIn train Core training process
#' @import nloptr
#' @import dplyr
#' @noRd
train_inc <- function(ts_input, ts_output, iter, log, param_selection = "best_bic"){

  # help function to compute model metrics to store training history
  append_hist <- function(train_hist, mix, param, operation_str, optim_str){
    mod <- createSWR(param = param, mix = mix)
    predict(mod, newdata = ts_input, log = log) -> p
    train_hist <- rbind(train_hist,
                        cbind(
                          operation = operation_str,
                          optimization = optim_str,
                          loglik = loglik(prediction = p,
                                          reference = ts_output),
                          aic = AIC(mod, ts_input = ts_input, ts_output = ts_output),
                          bic = BIC(mod, ts_input = ts_input, ts_output = ts_output),
                          eval_all(p, ts_output)
                        )
    )
    return(train_hist)
  }
  train_hist <- c()
  param_hist <- list()

  # parameter initialization
  param <- matrix(,nrow = 0, ncol = 2)
  mix <- c()

  # iteration counter
  i = 1
  stop = FALSE

  while(!stop){

    # heuristic initialization
    if(i > 1){
      delta_opts <- c(#0, # minimum point - 1
                      min(param[,1]) / 2,
                      param[-nrow(param),1] + diff(param[,1]) / 2, # center point between kernels
                      max(param[,1] + 5)
                      )
      mix_new <- sum(mix) * c(mix / (sum(mix) + 1), 1 / (sum(mix) + 1) )
      sigma_new <- rep(1, i)#c(param[,2], 1)
        #rep(sum(param[,2]) / i, i) # use previous sigma and distribute upon i windows
    }
    else{
      delta_opts = 1
      mix_new = 1
      sigma_new = 1
    }

    delta <- param[,1]
    delta_bic <- c()
    param_ <- list()
    mix_ <- list()
    for(delta_new in delta_opts){
      param <- cbind(delta = c(delta, delta_new),
                     sigma = sigma_new)
      mix <- mix_new

      # RUN OPTIMIZATION ALGORITHM
      opts <- list("algorithm"="NLOPT_LN_BOBYQA",
                   "xtol_rel" = 0,
                   "ftol_rel" = 0,
                   "xtol_abs" = 0,
                   "ftol_abs" = 1e-8,
                   "maxeval" = 1e4)

      # perform optimization
      res <- nloptr(
              x0 = encodeParam(mix = mix, param = param),
              eval_f = function(x){
               param_list <- decodeParam(x, intercept = FALSE)
               mix <- param_list$mix
               param <- param_list$param
               return(
                 log(error(ts_input, ts_output, param, mix, log)) # log for numerical stability
               )},
              lb = rep(0, length(mix) + length(param)),
              opts = opts
            )
      message <- res$message
      res <- res$solution %>%
            decodeParam(intercept = FALSE)
      mix <- res$mix
      param <- res$param

      # reorder windows by delta
      if(nrow(param) > 1){
        o <- order(param[,1])
        param <- param[o,]
        mix <- mix[o]
      }

      r <- createSWR(mix = mix,
                     param = param)
      delta_bic <- c(delta_bic, BIC(r, ts_input = ts_input, ts_output = ts_output))
      param_ <- append(param_, list(param))
      mix_ <- append(mix_, list(mix))
    }

    param <- param_[[which.min(delta_bic)]]
    mix <- mix_[[which.min(delta_bic)]]

    # add parameter history entry
    param_hist <- append(param_hist, list(list(mix = mix, param = param)))

    # add performance history entry
    train_hist <- append_hist(train_hist, mix, param, paste("iteration", i), message)

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
  #mix <- param_hist[[best_ind]]$mix
  #param <- param_hist[[best_ind]]$param
  #train_metrics <- train_hist[best_ind,-1]

  return(createSWR(param_hist[[best_ind]],
                   train_hist = train_hist,
                   train_metrics = train_hist[best_ind,-1],
                   param_hist = param_hist))
}

#' @title Train model
#' @description Trains an `SWR` model based on input and target time series data.
#' @details
#' The training procedure implements an iterative algorithm described in \insertCite{schrunner2023gaussian}{SlidingWindowReg}. A new window is added in each iteration, hence the number of windows equals to the iteration counter.
#' Input and output time series are provided in `ts_input` and `ts_output`, respectively. Both are required to have equal lengths.
#' The optimization is performed using the BOBYQA algorithm \insertCite{powell2009bobyqa}{SlidingWindowReg}, implemented in `nloptr` \insertCite{nloptr}{SlidingWindowReg}. As training hyperparameter, `iter` indicates the number of iterations, which equals to the maximum number of windows selected by the model.
#' Parameters `return` and `param_selection` indicate which criterion should be used to determine the number of windows; option are: `return="all"` (no hyperparameter selection) or `return="best"`, which allows one of the following options:
#' - `param_selection="best_aic"`: select model with lowest AIC,
#' - `param_selection="best_bic"`: select model with lowest BIC,
#' - `param_selection="best_rmse"`: select model with lowest RMSE.
#' Arguments `runs`, `parallel` are deprecated and should not be used.
#' @param ts_input a vector or ts object containing the input time series
#' @param ts_output a vector or ts object (on the same time scale as ts_input) containing the target time series
#' @param iter number of iterations (maximum number of windows)
#' @param runs `r lifecycle::badge("deprecated")` number of independent model runs; no longer supported due to deterministic window initialization
#' @param log whether a log-linear model should be used
#' @param parallel `r lifecycle::badge("deprecated")` should the runs be computed in parallel? If FALSE, all runs are computed in serial. If TRUE, all runs are computed in parallel with a maximum number of cores. If a scalar is provided, the number of cores is set manually. No longer supported for single-run models
#' @param return either "best" (best model run is returned), or "all" (all model runs are returned)
#' @param param_selection either "max" (maximum number of windows), or "best_rmse", "best_aic", or "best_bic" to optimize RMSE, AIC, or BIC, respectively
#' @examples
#' # train a model based on one year of observations
#' set.seed(42)
#' data(sampleWatershed)
#' mod <- trainSWR(sampleWatershed$rain[1:365],
#'                 sampleWatershed$gauge[1:365],
#'                 iter = 2)
#' summary(mod)
#' @references \insertAllCited{}
#' @import pbapply
#' @import parallel
#' @importFrom lifecycle deprecate_warn
#' @export
trainSWR <- function(ts_input, ts_output, iter = 5, runs, log = FALSE,
                  parallel, return = "best", param_selection = "best_aic"){

  if(length(ts_input) != length(ts_output)){
    stop("Error: input and output must be vectors of the same lengths")
  }

  if(!(return %in% c("best", "all"))){
    return = "best"
  }

  if (exists("runs")) {
    lifecycle::deprecate_warn(
      when = "0.1.1",
      what = "trainSWR(runs)",
      details = "Multiple model runs are not useful with deterministic window initializations."
    )
  }

  if (exists("parallel")) {
    lifecycle::deprecate_warn(
      when = "0.1.1",
      what = "trainSWR(parallel)",
      details = "Parallel model runs are not useful with deterministic window initializations."
    )
  }

  # create list of runs to perform
  #init_list <- 1 : runs # parameter list

  # train models
  # if(parallel == TRUE || is.numeric(parallel)){
  #   # initialize cluster
  #   if(is.numeric(parallel)){
  #     n_cores = min(parallel, parallel::detectCores() - 1)
  #   }
  #   else{
  #     n_cores = parallel::detectCores() - 1
  #   }
  #   cl <- parallel::makeCluster(n_cores)
  #   clusterExport(cl, list("ts_input", "ts_output", "log", "iter", "param_selection"), envir = environment())
  #   clusterExport(cl, list("error"), envir = environment())
  #
  #   # run computation
  #   res <- pbapply::pbreplicate(runs,
  #                               train_inc(
  #                                 ts_input = ts_input,
  #                                 ts_output = ts_output,
  #                                 iter = iter,
  #                                 log = log,
  #                                 param_selection = param_selection),
  #                               simplify = FALSE,
  #                               cl = cl)
  #
  #   # close cluster
  #   parallel::stopCluster(cl)
  # } else {
  #   # run computation
  runs <- 1
  res <- pbapply::pbreplicate(runs,
                                train_inc(
                                  ts_input = ts_input,
                                  ts_output = ts_output,
                                  iter = iter,
                                  log = log,
                                  param_selection = param_selection),
                                simplify = FALSE)
  #}

  # add fitted & residuals to list objects
  res <- lapply(1:length(res), function(x){
    r <- res[[x]]
    r$fitted <- predict_target(ts_input, r$mix, r$param)
    r$residuals <- ts_output - r$fitted
    return(r)})

  # reshape as array
  dim(res) <- runs
  names(res) <- paste("run",1:runs)

  if(return == "all"){
    return(res)
  }
  else{
    if(param_selection == "best_rmse" || param_selection == "max"){
      best_ind <- which.min(
        sapply(res, function(x){return(x$train_metrics$rmse)}))
    }
    else if(param_selection == "best_aic"){
      best_ind <- which.min(
        sapply(res, function(x){return(x$train_metrics$aic)}))
    }
    else if(param_selection == "best_bic"){
      best_ind <- best_ind <- which.min(
        sapply(res, function(x){return(x$train_metrics$bic)}))
    }
    else{
      stop("param_selection method unknown!")
    }
    res <- res[[best_ind]]
    return(res)
  }
}
