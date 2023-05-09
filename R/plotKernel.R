#' @title Return and plot accumulated kernel
#' @description Return and plot the accumulated kernel, consisting of one or multiple windows
#' @inheritParams createSWR
#' @param kernel_type a String indicating whether (a) all single windows (option: "single"), or (b) the combined kernel (option "combined")
#' @param weighted if TRUE, windows are weighted with mix parameters; default: TRUE
#' @examples
#' param <- cbind(
#'   delta = c(0, 10),
#'   sigma = c(2, 3))
#' mix <- rep(1, ncol(param))
#' mod <- createSWR(param = param, mix = mix)
#'
#' get_kernel(param = param, mix = mix)
#' coef(mod)
#'
#' plot_kernel(param = param, mix = mix)
#' plot(mod)
#' @export
get_kernel <- function(param, mix = NULL, kernel_type = "single", weighted = TRUE){

  if(!is.matrix(param) || ncol(param) != 2){
    stop("Error in get_kernel: param must be a matrix with 2 columns")
  }
  if(weighted) {
    if(is.null(mix)){
      # set mix to default
      mix <- rep(1, nrow(param))# VERSION WITH INTERCEPT: rep(1, nrow(param) + 1)
    } else if(!is.vector(mix) || length(mix) != (nrow(param))){# VERSION WITH INTERCEPT: nrow(param) + 1
      stop("Error in get_kernel: provided parameters are not consistent")
    }
  } else{
    mix <- rep(1, nrow(param))# VERSION WITH INTERCEPT: rep(1, nrow(param) + 1)
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

  # return according to kernel_type
  if(kernel_type == "single"){
    return(kernels * mix) # VERSION WITH INTERCEPT: mix[-1]
  } else if(kernel_type == "combined"){
    return(mix %*% kernels)# VERSION WITH INTERCEPT: mix[-1]
  }
  else{
    stop("Error in get_kernel: unknown kernel type")
  }
}

#' @describeIn get_kernel return the kernel of an `SWR` model
#' @inheritParams summary.SWR
#' @importFrom methods is
#' @export
coef.SWR <- function(object, ...){

  if(!is(object, "SWR")){
    stop("Wrong class of object")
  }

  return(get_kernel(param = object$param,
                    mix = object$mix,
                    ...))
}


#' @rdname get_kernel
#' @param colnames a vector of column names
#' @param rownames a vector of row names
#' @inheritParams plot_kernel
#' @noRd
plot_multiple_kernels <- function(list, colnames = NULL, rownames = NULL, kernel_type = "single", weighted = TRUE,
                                  xlim = NULL, include_text = TRUE){

  if(is.null(colnames)){
    colnames <- rep(1, length(list))
  }
  else{
    colnames <- colnames
  }
  if(is.null(rownames)){
    if(is.null(names(list))){
      rownames <- 1:length(list)
    }
    else{
      rownames <- names(list)
    }
  }
  rownames <- factor(rownames, levels = unique(rownames))
  colnames <- factor(colnames, levels = unique(colnames))

  if(is.null(xlim)){
    xlim <- c(-50,0)
  }

  # create data.frame
  d <- data.frame(x = c(),
                  time = c(),
                  window = c(),
                  rownames = c(),
                  colnames = c())

  for(i in 1:length(list)){
    # get kernel
    kernel <- get_kernel(list[[i]]$param, list[[i]]$mix, kernel_type, weighted)
    # produces warning due to removed names
    if(length(kernel) > 0){
      suppressWarnings({
        d <- rbind(
          d,
          data.frame(
            x = as.vector(t(kernel)),
            time = -(ncol(kernel) - 1) : 0,
            window = ordered(rep(1:nrow(kernel), each = ncol(kernel))),
            n_windows = rep(nrow(list[[i]]$param), ncol(kernel) * nrow(kernel)),
            delta = -rep(list[[i]]$param[,1], each = ncol(kernel)),
            rownames = rownames[i],
            colnames = colnames[i]
          )
        )
      })
    }
  }

  if(nrow(d) > 0){
    # avoid package compilation warning
    time <- x <- window <- delta <- n_windows <- NULL

    if(kernel_type == "single"){
      p <- ggplot(d,
                  aes(x = time,
                      y = x,
                      color = window,
                      group_by = window)) +
        scale_color_brewer(palette = "Dark2")
    } else if(kernel_type == "combined"){
      p <- ggplot(d,
                  aes(x = time,
                      y = x))
    }

    if(length(unique(rownames)) > 1){
      if(length(unique(colnames)) > 1){
        p <- p + facet_grid(rownames~colnames)
      }
      else{
        p <- p + facet_grid(rownames~.)
      }
    } else{
      if(length(unique(colnames)) > 1){
        p <- p + facet_grid(.~colnames)
      }
    }

    # add formatting instructions
    p <- p +
      geom_point() +
      geom_line(size = 1) +
      geom_vline(aes(xintercept = delta),
                 linetype = "dotted",
                 color = "black",
                 size = 0.8) +
      theme_bw() +
      theme(text = element_text(size = 20),
            legend.position = "bottom") +
      xlim(xlim)

    if(include_text){
      p <- p +
        geom_text(aes(label = paste0("no. windows = ", n_windows)),
                  x = min(d$time),
                  y = max(d$x),
                  color = "black",
                  hjust = 0,
                  vjust = 1)
    }

    return(p)
  }
  else{
    return(ggplot())
  }
}


#' @title Plot accumulated kernel
#' @rdname get_kernel
#' @param list a list containing multiple trained SlidingWindowReg models
#' @param xlim vector with lower / upper bound of the x axis to print the kernel
#' @param include_text if TRUE, plots will be annotated
#' @import ggplot2
#' @export
plot_kernel <- function(list = NULL, param = NULL, mix = NULL, kernel_type = "single", weighted = TRUE, xlim = NULL, include_text = TRUE){

  if(is.null(list)){
    if(is.null(param)){
      stop("Either list or param must be specified")
    } else {
      list <- list(
                list(param = param,
                     mix = mix)
      )
    }
  } else {
    if(!is.null(param)){
      warning("Both list and param specified - param will be ignored.")
    }
  }

  if(!is.null(dim(list))){
    rownames <- rep(dimnames(list)[[1]], prod(dim(list)[-1]))
    colnames <- rep(dimnames(list)[[2]], each = prod(dim(list)[-2]))
  } else{
    rownames <- colnames <- NULL
  }

  return(
    plot_multiple_kernels(list = list,
                          colnames = colnames,
                          rownames = rownames,
                          kernel_type = kernel_type,
                          weighted = weighted,
                          xlim = xlim,
                          include_text = include_text)
  )
}


#' Plot an `SWR` model
#' @describeIn get_kernel plot the kernel of an `SWR` model
#' @param x an `SWR` model object
#' @param type either "kernel" to produce a plot of the window kernels, or "prediction" to plot the model predictions, see \link{plot_prediction}
#' @param ... further plotting parameters, see  \link{plot_kernel} or \link{plot_prediction}, respectively
#' @importFrom methods is
#' @export
plot.SWR <- function(x, type = "kernel", ...){

  if(!is(x, "SWR")){
    stop("Wrong class of object")
  }

  if(type == "kernel"){
    return(plot_kernel(param = x$param,
                     mix = x$mix,
                     ...))
  }
  else if(type == "prediction"){
    pred <- predict(x, ...)

    return(plot_prediction(prediction = pred, ...))
  }
  else{
    stop("Unknown plot type")
  }


}
