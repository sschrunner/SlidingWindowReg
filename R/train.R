#' @title Train model window parameters
#' @description trains the component parameters based on time series input, output, mixture parameter, and an initialization
#' @import GA
train_param <- function(ts_input, ts_output, mix, param0, norm = rmse, popSize = 1e2, maxiter = 1e1){
  # param ... list of parameter sets (1 per component), each containing sd, kernel_rad, and mean_lag

  # encode/decode integer to/from 8-bit decimal
  encode <- function(x){
    return(as.vector(sapply(x, GA::decimal2binary, length = 8)))
  }
  decode <- function(x){
    return(apply(matrix(x, nrow = 8), 2, GA::binary2decimal))
  }

  param_vec <- as.vector(param0)
  param <- ga(type = "binary",
                  fitness = function(x){
                    x_int <- decode(x)
                    x_mat <- matrix(x_int, ncol = 2)
                    if(any(x_mat[-1,1] - x_mat[-nrow(x_mat),1] < 0)){
                      return(-Inf)
                    }
                    else{
                      return(-norm(predict(ts_input, mix, x_mat), ts_output))
                    }
                  },
                  nBits = length(param_vec) * 8, # 9-bit encoding
                  suggestions = t(encode(param_vec)),
                  popSize = popSize,
                  maxiter = maxiter
  )@solution[1,] # first solution
  param <- decode(param)
  param <- matrix(param, ncol = 2)
  dimnames(param) <- dimnames(param0)

  return(param)
}

#' @title Train model mixture parameters
#' @description trains the mixture component given the time series input, output, component parameters, and an initialization
#' @import nloptr
train_mix <- function(ts_input, ts_output, mix0, param, norm = rmse){
  opts <- list("algorithm"="NLOPT_LN_COBYLA",
               "xtol_rel"=1.0e-8)
  res <- nloptr(x0 = mix0,
                eval_f = function(x){
                  return(norm(predict(ts_input, x, param), ts_output))
                },
                lb = rep(0, length(mix0)),
                opts = opts
  )$solution

  return(res)
}

#' @title Train model
#' @description combines both training steps
#' @examples
#' set.seed(42)
#' param <- cbind(
#'   delta = c(1,4),
#'   kernel_rad = c(1,3))
#' mixture <- rep(1,ncol(param) + 1)
#' optim_norm = rmse
#' train_model(sampleWatershed$rain, sampleWatershed$gauge, mixture, param, iter = 3)
#' @export
train_model <- function(ts_input, ts_output, mixture, param, iter = 2){
  rmse_hist <- c()
  l0_hist <- c()
  psnr_hist <- c()

  predict(ts_input, mixture, param) -> p
  rmse_hist <- c(rmse_hist, rmse(p, ts_output))
  l0_hist <- c(l0_hist, l0_norm(p, ts_output))
  psnr_hist <- c(psnr_hist, psnr(p, ts_output))

  for(i in 1:iter){
    print(paste0("ITERATION: ",i))
    train_param(ts_input, ts_output, mix = mixture, param0 = param, norm = optim_norm) -> param
    predict(ts_input, mixture, param) -> p
    rmse_hist <- c(rmse_hist, rmse(p, ts_output))
    l0_hist <- c(l0_hist, l0_norm(p, ts_output))
    psnr_hist <- c(psnr_hist, psnr(p, ts_output))

    train_mix(ts_input, ts_output, mix0 = mixture, param = param, norm = optim_norm) -> mixture
    predict(ts_input, mixture, param) -> p
    rmse_hist <- c(rmse_hist, rmse(p, ts_output))
    l0_hist <- c(l0_hist, l0_norm(p, ts_output))
    psnr_hist <- c(psnr_hist, psnr(p, ts_output))
  }
  train_param(ts_input, ts_output, mix = mixture, param0 = param, norm = optim_norm) -> param
  predict(ts_input, mixture, param) -> p
  rmse_hist <- c(rmse_hist, rmse(p, ts_output))
  l0_hist <- c(l0_hist, l0_norm(p, ts_output))
  psnr_hist <- c(psnr_hist, psnr(p, ts_output))

  return(list(mixture = mixture, param = param,
              hist = data.frame(rmse = rmse_hist, l0_norm = l0_hist, psnr = -psnr_hist)))
}
