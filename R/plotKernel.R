#' @title Return and plot accumulated kernel
#' @description Return and plot the accumulated kernel, consisting of one or multiple windows
#' @inheritParams predict
#' @param type a String indicating whether (a) all single windows (option: "single"), or (b) the combined kernel (option "combined")
#' @param weighted if TRUE, windows are weighted with mix parameters; default: TRUE
#' @examples
#' param <- cbind(
#'   delta = c(0, 10),
#'   sigma = c(2, 3))
#' mix <- rep(1, ncol(param) + 1)
#' get_kernel(param = param, mix = mix)
#' plot_kernel(param = param, mix = mix)
#' @export
get_kernel <- function(param, mix = NULL, type = "single", weighted = TRUE){

  if(!is.matrix(param) || ncol(param) != 2){
    stop("Error in get_kernel: param must be a matrix with 2 columns")
  }
  if(weighted) {
    if(is.null(mix)){
      # set mix to default
      mix <- rep(1, nrow(param) + 1)
      warning("No weights provided in get_kernel, set to 1")
    } else if(!is.vector(mix) || length(mix) != (nrow(param) + 1)){
      stop("Error in get_kernel: provided parameters are not consistent")
    }
  } else{
    mix <- rep(1, nrow(param) + 1)
  }

  # compute kernels
  kernels <- apply(param, 1, build_gaussian_kernel, simplify = FALSE)

  # compute kernel length and initialize combined kernel
  kernel_length <- ifelse(length(kernels) > 0, max(sapply(kernels, length)), 0)
  kernels <- lapply(kernels, function(x){return(c(rep(0, kernel_length - length(x)), x))})
  kernels <- do.call(rbind, kernels)

  # modify rownames
  if(!is.null(kernels)){
    rownames(kernels) <- paste0("kernel", 1:nrow(kernels))
    colnames(kernels) <- (- (kernel_length - 1)) : 0
  }
  else{
    return(numeric(0))
  }

  # return according to type
  if(type == "single"){
    return(kernels * mix[-1])
  } else if(type == "combined"){
    return(mix[-1] %*% kernels)
  }
  else{
    stop("Error in get_kernel: unknown type")
  }
}

#' @title Plot accumulated kernel
#' @rdname get_kernel
#' @import ggplot2
#' @export
plot_kernel <- function(param, mix = NULL, type = "single", weighted = TRUE){

  # get kernel
  kernel <- get_kernel(param, mix, type, weighted)
  if(length(kernel) == 0){
    warning("Cannot plot model with 0 windows - returning empty plot")
    return(ggplot())
  }

  # create data.frame
  d <- data.frame(
      x = as.vector(t(kernel)),
      time = -(ncol(kernel) - 1) : 0,
      window = ordered(rep(1:nrow(kernel), each = ncol(kernel)))
    )

  if(type == "single"){
    p <- ggplot(d,
           aes(x = time,
               y = x,
               color = window,
               group_by = window)) +
         scale_color_brewer(palette = "Dark2")
  } else if(type == "combined"){
    p <- ggplot(d,
            aes(x = time,
                y = x))
  }

  # add formatting instructions
  p <- p +
    geom_point() +
    geom_line(size = 1) +
    theme_bw() +
    theme(text = element_text(size = 25),
          legend.position = "top") +
    geom_vline(xintercept = -param[,1],
               linetype = "dashed")

  return(p)
}
