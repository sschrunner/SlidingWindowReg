#' @title Convolve time series with discrete kernel
#' @description convolves the time series with a given kernel
convolve_window_disc <- function(ts, kernel, delta){
  ts_length <- length(ts)
  kernel_size <- length(kernel)
  if(kernel_size %% 2 != 1){
    stop("kernel must have odd length")
  }
  kernel_radius <- (kernel_size - 1) / 2
  kernel_sum <- sum(kernel)

  offset <- kernel_radius + delta
  res <- rep(0, ts_length)
  for(i in (kernel_radius+1) : (ts_length - delta)){
    res[i + delta] <- sum(kernel * ts[(i-kernel_radius) : (i+kernel_radius)])
  }

  return(res)
}

#' @title Create Gaussian kernel
#' @description creates a discrete Gaussian-shaped kernel (vector)
gauss_kernel_disc <- function(kernel_rad, sd = NULL, truncation = 0){
  x <- seq(-kernel_rad, kernel_rad)
  sd = ifelse(kernel_rad == 0, 1, kernel_rad / 3)
  kernel <- dnorm(x, mean = 0, sd = sd)

  # truncation: used to truncate kernel, if delta is smaller than kernel_rad
  if(truncation > 0){
    if(truncation > (length(kernel)-1)){
      stop("truncation larger than kernel size!")
    }
    else if(truncation > kernel_rad){
      warning("large truncation!")
    }
    kernel[(length(kernel) - truncation + 1) : length(kernel)] <- 0
  }

  kernel <- kernel / sum(kernel, na.rm = TRUE)

  return(kernel)
}

#' @title Convolve time series with contiuous kernel
#' @description convolves the time series with a given kernel
convolve_window_cont <- function(ts, kernel){
  ts_length <- length(ts)
  kernel_length <- length(kernel)

  res <- rep(0, ts_length)
  for(i in (kernel_length) : ts_length){
    res[i] <- sum(kernel * ts[(i - kernel_length + 1) : i])
  }

  return(res)
}

#' @title Create Gaussian kernel
#' @description creates a continuous Guassian-shaped kernel (vector)
gauss_kernel_cont <- function(delta, sigma){
  # delta, sigma continuous
  kernel_rad <- 3*sigma
  kernel_ind <- c(-ceiling(delta + kernel_rad),
                  min(-floor(delta - kernel_rad), 0))

  x <- seq(kernel_ind[1], 0)
  bins <- seq(kernel_ind[1] - 0.5, kernel_ind[2] + 0.5)
  kernel <- diff(pnorm(bins, mean = -delta, sd = sigma))
  kernel <- c(kernel, rep(0, length(x) - length(kernel)))
  names(kernel) <- x

  kernel <- kernel / sum(kernel, na.rm = TRUE)

  return(kernel)
}

#' @title Convolve time series with Gaussian kernel
#' @description convolves the time series with a Gaussian kernel
convolve_window_gaussian <- function(ts, param){
  #kernel <- gauss_kernel(kernel_rad = param[2], truncation = max(c(param[2] - param[1], 0))) # 2-parameter version: length = +/- 3 sigma
  kernel <- gauss_kernel_cont(delta = param[1], sigma = param[2])
  return(convolve_window_cont(ts, kernel))
}

#' @title Predict target variable
#' @description predicts the target variable given a time series of inputs, and trained parameters
#' @export
predict <- function(ts, mix, param){
  offset <- mix[1]
  conv <- apply(param, 1, convolve_window_gaussian, ts = ts)
  return(offset + as.vector(conv %*% mix[-1]))
}
