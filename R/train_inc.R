#' @title Train model window parameters
#' @description trains the component parameters based on time series input, output, mixture parameter, and an initialization
#' @import GA
train_param <- function(ts_input, ts_output, mix, param0, param_fixed = NULL, popSize = 1e2, maxiter = 1e1){
  # param ... list of parameter sets (1 per component), each containing sd, kernel_rad, and mean_lag

  # encode/decode integer to/from 8-bit decimal
  encode <- function(x){
    return(as.vector(sapply(x, GA::decimal2binary, length = 8)))
  }
  decode <- function(x){
    return(apply(matrix(x, nrow = 8), 2, GA::binary2decimal))
  }

  param_vec <- as.vector(param0)

  param <- GA::ga(type = "binary",
                  fitness = function(x){
                    x_int <- decode(x)
                    x_mat <- matrix(x_int, ncol = 2)
                    x_mat <- rbind(param_fixed, x_mat)
                    #if(any(x_mat[-1,1] - x_mat[-nrow(x_mat),1] < 0)){
                    #  return(-Inf)
                    #}
                    #else{
                      return(-rmse(predict(ts_input, mix, x_mat), ts_output))
                    #}
                  },
                  nBits = length(param_vec) * 8, # 9-bit encoding
                  suggestions = t(encode(param_vec)),
                  popSize = popSize,
                  maxiter = maxiter,
                  monitor = FALSE
  )
  param <- param@solution[1,] # first solution


  param <- decode(param)
  param <- matrix(param, ncol = 2)
  dimnames(param) <- dimnames(param0)

  param <- rbind(param_fixed, param)

  return(param)
}

#' @title Train model mixture parameters
#' @description trains the mixture component given the time series input, output, component parameters, and an initialization
#' @import nloptr
train_mix <- function(ts_input, ts_output, mix0, param, lambda = 0){
  # lambda ... L1 regularization
  opts <- list("algorithm"="NLOPT_LN_COBYLA",
               "xtol_rel"=1.0e-8)
  res <- nloptr(x0 = mix0,
                eval_f = function(x){
                  return(rmse(predict(ts_input, x, param), ts_output) + lambda * sum(abs(x)))
                },
                lb = rep(0, length(mix0)),
                opts = opts
  )$solution

  return(res)
}

#' train an elementary model
train_inc <- function(ts_input, ts_output, iter = 5){

  param <- matrix(,nrow = 0, ncol = 2)
  mix = 1

  append_hist <- function(train_hist, operation_str){
    predict(ts_input, mixture, param) -> p
    train_hist <- rbind(train_hist,
                        data.frame(operation = operation_str,
                              rmse = rmse(p, ts_output),
                              l0 = l0_norm(p, ts_output),
                              psnr = psnr(p, ts_output))
    )
    return(train_hist)
  }
  train_hist <- c()

  i = 1
  while(i < iter & all(mixture[-1] > 0)){
    # new component
    new_param <- cbind(sample(setdiff(0:255, param[,1]), size = 1),
                       sample(0:255, size = 1))
    mixture <- c(mixture, 1)

    train_param(ts_input, ts_output, mix = mixture, param0 = new_param, param_fixed = param) -> param
    train_hist <- append_hist(train_hist, paste0("train param", i))

    train_mix(ts_input, ts_output, mix0 = mixture, param = param, lambda = 1) -> mixture
    train_hist <- append_hist(train_hist, paste0("train mixture", i))
    i <- i+1
  }
  train_hist <- append_hist(train_hist, paste0("end after iteration", i))
  return(list(mixture = mixture,
              param = param,
              hist = train_hist))
}

#' @title Train model
#' @description combines both training steps
#' @examples
#' set.seed(42)
#' param <- cbind(
#'   delta = c(1,4),
#'   kernel_rad = c(1,3))
#' mixture <- rep(1,ncol(param) + 1)
#' train(sampleWatershed$rain, sampleWatershed$gauge, mixture, param, iter = 2, runs = 2)
#' @import pbapply
#' @import parallel
#' @export
train <- function(ts_input, ts_output, mixture, param, iter = 2, runs = 10){
  n_cores = parallel::detectCores()
  cl <- parallel::makeCluster(n_cores - 1)
  clusterExport(cl, list("param", "mixture", "ts_input", "ts_output", "iter"), envir = environment())
  #clusterEvalQ(cl, library("GA"))
  #clusterEvalQ(cl, library("nloptr"))

  res <- pbapply::pbreplicate(runs, train_inc(ts_input, ts_output, iter), cl = cl)
  parallel::stopCluster(cl)

  return(res)
}
