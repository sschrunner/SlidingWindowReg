# help functions to encode parameter set as vector
encodeParam <- function(mix, param){
  return(
    c(mix, as.vector(param))
  )
}

# help function to decode parameter set from vector
decodeParam <- function(x, intercept = FALSE){
  num_win = ifelse(intercept, (length(x) - 1), length(x)) / 3
  num_mix = ifelse(intercept, num_win + 1, num_win)
  mix = x[1 : num_mix]
  param = matrix(x[- (1 : num_mix)],
                 ncol = 2,
                 nrow = num_win)
  return(
    list(
      mix = mix,
      param = param
    )
  )
}

# help function to evaluate model error
#' @importFrom stats dist
error <- function(ts_input, ts_output, param, mix, lambda, log, lambda1 = 1){
    return(rmse(predict(ts_input, mix, param, log), ts_output)
    #return(BIC(predict(ts_input, mix = mix, param = param), ts_output, list(param = param, mix = mix))
  )
}

# performs continuous optimization with respect to mixing and window parameters
#' @import nloptr
#' @import rBayesianOptimization
train_both <- function(ts_input, ts_output, mix0, param0, lambda, log){

  # specify algorithm (conjugated gradient) and tolerance
  opts <- list("algorithm"="NLOPT_LN_BOBYQA",
               "xtol_rel" = 0,
               "ftol_rel" = 0,
               "xtol_abs" = 1e-8,
               "maxeval" = 1e5)

  # perform optimization
  res <- nloptr(
    x0 = encodeParam(mix = mix0, param = param0),
    eval_f = function(x){
      param_list <- decodeParam(x, intercept = FALSE)
      mix <- param_list$mix
      param <- param_list$param
      return(
        error(ts_input, ts_output, param, mix, lambda, log)
      )},
    lb = rep(0, length(mix0) + length(param0)),
    opts = opts
  )$solution

  return(res)
}

# train an elementary model
train_inc <- function(ts_input, ts_output, iter, lambda, log, improvement_thres = 0, param_selection = "best_nrmse"){

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
  mix = c() # VERSION WITH INTERCEPT: 1

  # iteration counter
  i = 1
  stop = FALSE

  param_hist <- list()

  while(!stop){
    print(paste0("ITERATION:", i))
    print(param_hist)

    # initialize whole delta parameters via Bayesian optimization
    bounds <- replicate(i, c(0, 100), simplify = FALSE)
    names(bounds) <- paste0("delta", 1:i)

    if(i > 1){
      init_pts <- as.data.frame(t(c(param[,1], 0))) # optimal param from last iteration
      colnames(init_pts) <- names(bounds)
      mix_new <- rep(sum(mix) / i, i)#VERSION WITH INTERCEPT: c(mix[1], rep(sum(mix[-1]) / i, i)) # use previous mixture and distribute upon i windows
      sigma_new <- rep(sum(param[,2]) / i, i) # use previous sigma and distribute upon i windows
    }
    else{
      init_pts <- NULL
      mix_new <- 1 # VERSION WITH INTERCEPT: c(mean(ts_output),1)
      sigma_new <- 1
    }

    print("starting Bayesian Optimization...")

    delta <- BayesianOptimization(FUN = function(...){
      delta0 <- cumsum(c(...))
      return(list(
        Score = -error(ts_input, ts_output, cbind(delta0, sigma_new), mix = mix_new, lambda = lambda, log = log),
        Pred = 0))
    },
    bounds = bounds,
    #init_grid_dt = init_pts,
    init_points = 30,
    n_iter = 10,
    verbose = TRUE#,
    #kernel = list(type = "matern", nu = 5/2) # default kernel produces errors
    )$Best_Par

    print("finished Bayesian Optimization...")

    print(delta)

    param <- cbind(cumsum(delta), sigma_new) # delta from Bayesian optimization, sigma = 1
    mix <- mix_new

    # set lambda to 0 for first window (no regularization)
    lambda0 <- ifelse(i == 1, 0, lambda)

    # train parameters
    print("starting training...")

    for(j in 1:3){
      p <- train_both(ts_input, ts_output, mix0 = mix, param0 = param, lambda = lambda0, log = log)
      param_list <- decodeParam(p)
      mix <- param_list$mix
      param <- param_list$param
    }

    print("finishing trainig...")

    # reshape and update parameters
    #param_list <- decodeParam(p)
    #mix <- param_list$mix
    #param <- param_list$param

    ### ADD OLS TRAINING ###
    #d <- cbind(data.frame(y = ts_output),
    #           apply(param,
    #                 1,
    #                 function(x){return(SlidingWindowReg::predict(ts_input, mix = c(0,1), param = t(x)))}))
    #mix <- coef(lm(y~., data = d))
    ###

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
    # stop <- (stop | any(mix[-1] == 0)) # stop if any coefficient is 0 # ACTIVATE AGAIN IF NEEDED

    # stop if gain in RMSE is low
    # if(i > 1){ # ACTIVATE AGAIN IF NEEDED
    #   rmse_prior <- train_hist[nrow(train_hist) - 1, "rmse"]
    #   rmse_current <- train_hist[nrow(train_hist), "rmse"]
    #   stop <- (stop | ((1 - rmse_current / rmse_prior) < improvement_thres)) # test if significant improvement
    # }

    # increment counter
    i <- i+1
  }

  # remove cancelled windows (mix-parameter 0)
  # rm_win <- which(mix[-1] == 0)
  # if(length(rm_win) > 0){
  #   mix <- mix[- (rm_win + 1)]
  #   param <- param[- rm_win, , drop = FALSE]
  # }

  # mix / param from best model (w.r.t. number of windows)
  ##### HYPERPARAMETER SELECTION #####
  if(param_selection == "best_nrmse"){
    best_ind <- which.min(train_hist$rmse)
    mix <- param_hist[[best_ind]]$mix
    param <- param_hist[[best_ind]]$param
    train_metrics <- train_hist[best_ind,-1]
  }
  else if(param_selection == "max"){
    last_ind <- length(train_hist$rmse)
    mix <- param_hist[[last_ind]]$mix
    param <- param_hist[[last_ind]]$param
    train_metrics <- train_hist[last_ind,-1]
  }
  else{
    stop("param_selection method unknown!")
  }
  #####

  return(
    list(mix = mix,
         param = param,
         lambda = lambda,
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
#' @param cv_fold number of folds for time series cross-validation; if 1, the full dataset is used for training
#' @param runs number of independent model runs
#' @param lambda a non-negative scalar or vector indicating the L1 regularization parameter(s)
#' @param log whether a log-linear model should be used
#' @param parallel should the runs be computed in parallel? If FALSE, all runs are computed in serial. If TRUE, all runs are computed in parallel with a maximum number of cores. If a scalar is provided, the number of cores is set manually.
#' @param return either "best" (best model run is returned), or "all" (all model runs are returned)
#' @examples
#' set.seed(42)
#' train(sampleWatershed$rain,
#'       sampleWatershed$gauge,
#'       iter = 2,
#'       cv_fold = 1,
#'       runs = 1,
#'       parallel = FALSE)
#' @import pbapply
#' @import parallel
#' @export
train <- function(ts_input, ts_output, iter = 10, cv_fold = 5, runs = 10, lambda = 0.1, log = FALSE,
                  parallel = TRUE, return = "best", param_selection = "best_nrmse"){

  if(length(ts_input) != length(ts_output)){
    stop("Error: input and output must be vectors of the same lengths")
  }

  if(!(return %in% c("best", "all"))){
    return = "best"
  }

  # split by years for cross-validation and create folds
  years <- ceiling(length(ts_input) / 365)
  if(years < cv_fold){
    warning("Number of years is smaller than cv_fold parameter; setting cv_fold to 1.")
    cv_fold = 1
  }

  if(cv_fold > 1){
    cv_init <- 1 # number of folds for initialization of time-series cross-validation
    cutoff_years <- floor(seq(1, years + 1, length.out = cv_fold + cv_init + 1))
    fold_assignment <- rep(1 : (cv_fold + cv_init), 365 * diff(cutoff_years))[1 : length(ts_input)]
    folds <- lapply(cv_init : (cv_init + cv_fold - 1), function(i){return(1:i)}) # train folds for time-series cross-validation
    test_folds <- lapply(folds, function(i){return(max(i)+1)}) # test folds for time-series cross-validation
  } else{
    fold_assignment <- rep(1, length(ts_input))
    folds <- list(1 : length(ts_input))
    test_folds <- NULL
  }
  stopifnot(all(sapply(folds, length) >= 1)) # each fold should contain at least 1 year

  # create list of runs to perform
  init_list <- expand.grid(run = 1 : runs, fold = 1 : cv_fold, lambda = lambda)[,c(2,3)] # parameter list (run, fold, lambda)

  # target function
  eval_fct <- function(x){ # lambda: regularization term to penalize close windows

    # training indices
    train_inds <- which(fold_assignment %in% folds[[ x["fold"] ]])
    test_inds <- which(fold_assignment %in% test_folds[[ x["fold"] ]])
    # train model
    res <- train_inc(ts_input = ts_input[train_inds],
                     ts_output = ts_output[train_inds],
                     iter = iter,
                     lambda = c(x["lambda"]),
                     log = log,
                     param_selection = param_selection)

    # compute test metrics, if cross-validation is performed
    if(!is.null(test_inds) && length(test_inds) > 0 &&
       all(res$param[,1] + 3 * res$param[,2] < (length(ts_input) - length(train_inds)))){
      pred <- predict(ts_input[test_inds],
                      mix = res$mix,
                      param = res$param,
                      log = log)
      if(length(unique(na.omit(pred))) > 0 && length(unique(na.omit(train_inds))) > 0){
        res$test_metrics <- eval_all(predict(ts_input[test_inds],
                                             mix = res$mix,
                                             param = res$param,
                                             log = log),
                                     ts_output[test_inds])
      } else{
        res$test_metrics <- c(rmse = NA)
      }

    } else{
      res$test_metrics <- c(rmse = NA)
    }
    return(res)
  }

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
    clusterExport(cl, list("ts_input", "ts_output", "log", "iter"), envir = environment())
    clusterExport(cl, list("folds", "test_folds", "fold_assignment"), envir = environment())
    clusterExport(cl, list("error"), envir = environment())

    # run computation
    res <- pbapply::pbapply(init_list, 1,
                            eval_fct,
                            cl = cl)

    # close cluster
    parallel::stopCluster(cl)
  } else {
    # run computation
    res <- pbapply::pbapply(init_list, 1,
                            eval_fct)
  }

  # add fold to list objects
  res <- lapply(1:length(res), function(x){
    r <- res[[x]]
    r$fold <- init_list$fold[x]
    r$fitted <- predict(ts_input, r$mix, r$param)
    r$residuals <- ts_output - r$fitted
    return(r)})

  # reshape as array
  dim(res) <- c(runs, cv_fold, length(lambda))
  dimnames(res) <- list(paste("run",1:runs), paste("fold",1:cv_fold), paste0("lambda=",lambda))

  if(return == "all"){
    return(res)
  }
  else{
    rmse <- apply(res, c(1,2,3), function(x){return(x[[1]]$train_metrics$rmse)})
    best_rmse <- which(rmse == min(rmse, na.rm = TRUE))
    if(length(best_rmse) > 1){
      warning("more than one optimal model - returning only first")
      best_rmse <- best_rmse[1]
    }
    return(res[best_rmse])
  }
}
