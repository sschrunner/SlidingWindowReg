#' @import nloptr
train_both <- function(ts_input, ts_output, mix0, param0, lambda = 0){
  # lambda ... L1 regularization
  opts <- list("algorithm"="NLOPT_LN_BOBYQA",
               "xtol_rel"=1.0e-8)

  res <- nloptr(x0 = c(mix0,as.vector(param0)),
                eval_f = function(x){
                  num_win = (length(x)-1)/3
                  mix =  x[1:(num_win+1)]
                  param = matrix(x[-(1:(num_win+1))], ncol = 2, nrow = num_win)
                  return(nrmse(predict(ts_input, mix, param), ts_output) + lambda * sum(abs(mix)))
                },
                lb = rep(0, length(mix0) + length(param0)),
                opts = opts
  )$solution

  return(res)
}

#' train an elementary model
train_inc <- function(ts_input, ts_output, iter = 5){

  param <- matrix(,nrow = 0, ncol = 2)
  mix = 1

  append_hist <- function(train_hist, mix, param, operation_str){
    predict(ts_input, mix, param) -> p
    train_hist <- rbind(train_hist,
                        cbind(
                          operation = operation_str,
                          eval_all(p, ts_output)
                        )
    )
    return(train_hist)
  }
  train_hist <- c()

  i = 1
  while(i <= iter & all(mix[-1] > 0)){
    if(i == 1){
      new_param <- cbind(0,
                        sample(0:255, size = 1))
    } else{
      new_param <- cbind(sample(setdiff(0:255, param[,1]), size = 1),
                         sample(0:255, size = 1))
    }
    # add new window
    mix <- c(mix, ifelse(i == 1, 1, 0))
    param <- rbind(param, new_param)

    lambda = ifelse(i == 1, 0, 0.1)

    # train params
    train_both(ts_input, ts_output, mix0 = mix, param0 = param, lambda = lambda) -> p

    # update params
    num_win = (length(p)-1)/3
    mix = p[1:(num_win+1)]
    param = matrix(p[-(1:(num_win+1))], ncol = 2, nrow = num_win)

    train_hist <- append_hist(train_hist, mix, param, paste0("train param", i))

    # reorder windows
    if(nrow(param) > 1){
      # reorder
      o <- order(param[,1])
      param <- param[o,]
      mix[-1] <- (mix[-1])[o]
    }

    i <- i+1
  }

  # remove empty windows
  rm_win <- which(mix[-1] == 0)
  if(length(rm_win) > 0){
    mix <- mix[-(rm_win+1)]
    param <- param[-rm_win,, drop = FALSE]
  }

  train_hist <- append_hist(train_hist, mix, param, paste0("end after iteration", i))
  return(list(mix = mix,
              param = param,
              hist = train_hist,
              metrics = train_hist[nrow(train_hist),]))
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
train <- function(ts_input, ts_output, iter = 2, runs = 10){
  n_cores = parallel::detectCores()
  cl <- parallel::makeCluster(n_cores - 1)
  clusterExport(cl, list("ts_input", "ts_output", "iter"), envir = environment())

  # DEACTIVEATED FOR BUG FIXING
  res <- pbapply::pbreplicate(runs, train_inc(ts_input, ts_output, iter), cl = cl)
  # ACTIVATE FOR BUG FIXING
  # res <- pbapply::pbreplicate(runs, train_inc(ts_input, ts_output, iter))

  parallel::stopCluster(cl)

  return(res)
}
