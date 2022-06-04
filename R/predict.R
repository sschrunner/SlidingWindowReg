#' Convolve time series with a given kernel
convolve_window <- function(ts, kernel){

  # compute lengths of ts and kernel
  ts_length <- length(ts)
  kernel_length <- length(kernel)

  # initialize prediction with 0
  res <- rep(0, ts_length)

  # convolve kernel
  for(i in (kernel_length) : ts_length){
    res[i] <- sum(kernel * ts[(i - kernel_length + 1) : i])
  }

  return(res)
}

#' Create Gaussian kernel
build_gaussian_kernel <- function(param){

  delta <- param[1]
  sigma <- param[2]

  # define kernel radius by 3*sigma
  kernel_rad <- 3 * sigma

  # define minimum / maximum index by delta +/- 3*sigma (rounded)
  kernel_ind <- c(- ceiling(delta + kernel_rad),
                  min(- floor(delta - kernel_rad), 0))

  # initialize kernel values with 0 and define bins
  x <- seq(kernel_ind[1], 0)
  bins <- seq(kernel_ind[1] - 0.5, kernel_ind[2] + 0.5)

  # compute kernel values for window by integrating over Gaussian distribution in each bin
  kernel <- diff(pnorm(bins, mean = - delta, sd = sigma))
  kernel <- c(kernel, rep(0, length(x) - length(kernel)))
  names(kernel) <- x

  # scale kernel (if truncated)
  kernel <- kernel / sum(kernel, na.rm = TRUE)

  return(kernel)
}

#' @title Predict target variable
#' @description predicts the target variable given a time series of inputs, and trained parameters
#' @param ts a vector or ts object
#' @param mix a vector of mixing parameters (beta)
#' @param param a matrix with 2 columns representing one window per row. The first column contains location parameters delta, the second column contains the standard deviation sigma.
#' @param log whether a log-linear model should be used
#' @export
predict <- function(ts, mix, param, log = FALSE){
  if(!is.vector(ts)){
    stop("Error in predict: ts must be a vector")
  }
  if(!is.vector(mix) || !is.matrix(param) || length(mix) != (nrow(param) + 1)){
    stop("Error in predict: provided parameters are not consistent")
  }

  # compute offset and convolution for each window
  offset <- mix[1]
  if(nrow(param) > 0){
    conv <- apply(param,
                  1,
                  function(x, ts){
                    kernel <- build_gaussian_kernel(x)
                    return(convolve_window(ts, kernel))
                  },
                  ts = ts)
    if(log){
      conv <- log(conv)
    }
    res <- offset + as.vector(conv %*% mix[-1])

  }
  else{
    res <- rep(offset, length(ts))
  }
  return(res)
}
