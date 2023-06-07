#' @title Create `SWR` model object
#' @description Constructor to create an `SWR` model object manually based on model parameters
#' @details
#' An `SWR` model object is an S3-class object, which contains at least two variables:
#' -  a matrix `param` with 2 columns representing one window per row. The first column contains location parameters delta (>=0), the second column contains the standard deviation sigma (>0),
#' -  a vector `mix` of regression parameters, representing one window each.
#' Thus, the number of windows is given by the number of rows in `param`, as well as by the number of elements in `mix`.
#' Additional variables can be appended to the `SWR` model object by using the `...` argument
#' @param list a list object to be converted to an `SWR` model object
#' @param mix a vector of mixing parameters (beta), see details
#' @param param a matrix with 2 columns, see details
#' @param ... further variables to be appended to the `SWR` model object
#' @export
createSWR <- function(list = NULL, param = NULL, mix = NULL, ...){

  if(!is.null(list)){
    try(return(createSWR(
                  param = list$param,
                  mix = list$mix,
                  ...)
    ))
    stop("Error in createSWR: list is not consistent with SWR object definition")
  }
  else{
    if(!is.vector(mix) || !is.matrix(param) || length(mix) != nrow(param)){ # VERSION WITH INTERCEPT: nrow(param) + 1
      stop("Error in createSWR: provided parameters are not consistent")
    }

    mod <- list(param = param,
                mix = mix)
    mod <- append(mod, list(...))
    class(mod) <- "SWR"

    return(mod)
  }
}
