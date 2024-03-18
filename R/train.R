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
#' @import dplyr
#' @import nloptr
#' @import rgenoud
#' @noRd
train_inc <- function(ts_input, ts_output, iter, log, param_selection = "best_bic", algorithm = "BOBYQA"){

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

  if(algorithm == "GENOUD"){
    # compute mean of output/input for scaling purposes
    mean_input    <- mean(ts_input, na.rm = T)
    mean_output   <- mean(ts_output, na.rm = T)
  }

  while(!stop) {


    if(algorithm == "GENOUD"){
      ## genetic optimization algorithm (genoud)
      nInits    <- 100      # popuation size
      inits     <- build_inits(nInits, i, mean_input, mean_output)
      # range of parameters
      ranges    <- matrix(rep(c(0,    50, # beta
                                0,    100, #delta
                                1/6,  50), i), ncol = 2, byrow = T)
    }
    else{
      # heuristic initialization
      if(i > 1){
        delta_opts <- c(min(param[,1]) / 2,
                        param[-nrow(param),1] + diff(param[,1]) / 2, # center point between kernels
                        max(param[,1] + 1),
                        max(param[,1] + 5),
                        max(param[,1] + 10)
        )
        mix_new <- sum(mix) * c(mix / (sum(mix) + 1), 1 / (sum(mix) + 1) )
        sigma_new <- rep(1, i)
      }
      else{
        delta_opts = 1
        mix_new = 1
        sigma_new = 1
      }
    }

    # initialization of temporary variables
    delta     <- param[,1]
    delta_bic <- c()
    param_    <- list()
    mix_      <- list()

    if(algorithm == "GENOUD"){
      res       <- genoud(fn  = function(x) {
                                  param_list <- decodeParam(x, intercept = FALSE)
                                  mix        <- param_list$mix
                                  param      <- param_list$param
                                  return(error(ts_input, ts_output, param, mix, log))
                                },
                          nvars                = i*3,
                          max.generations      = 50,
                          wait.generations     = 5,
                          starting.values      = inits,
                          boundary.enforcement = 2,
                          BFGSburnin           = 5,
                          pop.size             = nInits,
                          gradient.check       = F,
                          print.level          = 0,
                          solution.tolerance   = 0.1,
                          Domains              = ranges)
      message   <- NA
      res       <- res$par %>%
        decodeParam(intercept = FALSE)

      mix       <- res$mix
      param     <- res$param

      # reorder windows by delta
      if(nrow(param) > 1){
        o         <- order(param[,1])
        param     <- param[o,]
        mix       <- mix[o]
      }
      r         <- createSWR(mix   = mix,
                             param = param)
      delta_bic <- c(delta_bic, BIC(r, ts_input = ts_input, ts_output = ts_output))
      param_    <- append(param_, list(param))
      mix_      <- append(mix_, list(mix))

    }
    else{
      for(delta_new in delta_opts){
        param <- cbind(delta = c(delta, delta_new),
                       sigma = sigma_new)
        mix <- mix_new

        # RUN OPTIMIZATION ALGORITHM
        opts <- list("algorithm"="NLOPT_LN_BOBYQA",
                     "xtol_rel" = 0,
                     "ftol_rel" = 0,
                     "xtol_abs" = 0,
                     "ftol_abs" = 1e-3,
                     "maxeval" = 1e4)

        # perform optimization
        res <- nloptr(
          x0 = encodeParam(mix = mix, param = param),
          eval_f = function(x){
            param_list <- decodeParam(x, intercept = FALSE)
            mix <- param_list$mix
            param <- param_list$param
            return(
              error(ts_input, ts_output, param, mix, log)
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
    }

    param     <- param_[[which.min(delta_bic)]]
    mix       <- mix_[[which.min(delta_bic)]]

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

  return(createSWR(param_hist[[best_ind]],
                   train_hist = train_hist,
                   train_metrics = train_hist[best_ind,-1],
                   param_hist = param_hist))
}


#' Build a initialization matrix for genoud
#'
#'
#'
#' @param nInits        population size for genetic algorithm
#' @param nWin          number of windows to be used for SWR model
#' @param mean_input    mean of input time series
#' @param mean_output   mean of output time series
#'
#' @return a matrix where each row represent a member of pupulation and columns represent initial
#'         parameter values of the model
#' @noRd
build_inits <- function(nInits, nWin, mean_input, mean_output) {
  init0 <- data.frame(beta  = rep(mean_output/(nWin*mean_input), nInits),
                      delta = rep(1:(nInits/4), 4),
                      sigma = rep(c(1, 5, 10, 20), each = nInits/4))
  n     <- nrow(init0)
  inits <- init0
  i     <- nWin
  while (i > 1) {
    inds  <- sample(1:n, n, replace = F)
    inits <- cbind(inits, init0[inds,])
    i     <- i - 1
  }
  return(as.matrix(inits))
}



#' @title Train model
#' @description Trains an `SWR` model based on input and target time series data.
#' @details
#' The training procedure implements an iterative algorithm described in \insertCite{schrunner2023gaussian}{SlidingWindowReg}. A new window is added in each iteration, hence the number of windows equals to the iteration counter.
#' Input and output time series are provided in `ts_input` and `ts_output`, respectively. Both are required to have equal lengths.
#' The optimization is performed using the GENOUD algorithm \insertCite{mebanesekhon2011genoud}{SlidingWindowReg}, implemented in `rgenoud` \insertCite{rgenoud}{SlidingWindowReg}, or alternatively using the BOBYQA algorithm \insertCite{powell2009bobyqa}{SlidingWindowReg}, implemented in `nloptr` \insertCite{nloptr}{SlidingWindowReg}.
#' As training hyperparameter, `iter` indicates the number of iterations, which equals to the maximum number of windows selected by the model.
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
#' @param return `r lifecycle::badge("deprecated")` either "best" (best model run is returned), or "all" (all model runs are returned)
#' @param param_selection either "max" (maximum number of windows), or "best_rmse", "best_aic", or "best_bic" to optimize RMSE, AIC, or BIC, respectively
#' @param algorithm either "GENOUD" (genetic optimization using derivatives), or "BOBYQA" (bound optimization by quadratic approximation)
#' @returns an object of type `SWR` model
#' @examples
#' # load the sample dataset and train a model based on one year of observations
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
                  parallel, return = "best", param_selection = "best_bic",
                  algorithm = "GENOUD"){

  if(length(ts_input) != length(ts_output)){
    stop("Error: input and output must be vectors of the same lengths")
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

  if (exists("return")) {
    lifecycle::deprecate_warn(
      when = "0.1.1",
      what = "trainSWR(return)",
      details = "Return argument is unused."
    )
  }

  res <- train_inc(ts_input = ts_input,
                   ts_output = ts_output,
                   iter = iter,
                   log = log,
                   param_selection = param_selection,
                   algorithm = algorithm)

  # add fitted & residuals to list objects
  res$fitted <- predict_target(ts_input, res$mix, res$param)
  res$residuals <- ts_output - res$fitted

  return(res)
}

#' @title Cochrane-Orcutt procedure
#' @description Cochrane-Orcutt procedure to resolve auto-correlated residuals in `SWR` models.
#' @details
#' If an `SWR` model has auto-correlated residuals, the Cochrane-Orcutt procedure can be used to transform the data, such that auto-correlations are removed.
#' Afterwards, the `SWR` model is retrained on the transformed data. For details, see \insertCite{schrunner2023gaussian}{SlidingWindowReg}.
#' @inheritParams trainSWR
#' @param model an `SWR` model
#' @param ar number of autoregressive lags
#' @param ... parameters for re-training the model using \link{trainSWR}
#' @returns an object of type `SWR` model, see \link{createSWR}
#' @importFrom stats arima
#' @importFrom stats na.omit
#' @export
cochrane_orcutt <- function(model, ts_input, ts_output, ar = 1, ...){
  if(ar < 1){
    stop("ar parameter cannot be smaller than 1")
  }

  # estimate parameter phi from AR model
  phi <- arima(model$residuals,
               order = c(ar, 0, 0),
               include.mean = FALSE)$coef

  # transform ts_input and ts_output
  B <- function(ts, lag = 1){
    l <- length(ts)
    ts <- c( rep(NA, lag),
             ts[1 : (l - lag)])
    return(ts)
  }
  ts_input <- as.vector(na.omit(sapply(0:ar, B, ts = ts_input) %*% c(1, -phi)))
  ts_output <- as.vector(na.omit(sapply(0:ar, B, ts = ts_output) %*% c(1, -phi)))

  # re-run model training
  mod <- trainSWR(ts_input, ts_output, ...)
  mod$phi <- phi

  return(mod)
}
