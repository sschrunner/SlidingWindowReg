#' @title Create `SWR` model object
#' @description Constructor to create an `SWR` model object manually based on model parameters
#' @param list a list object to be converted to an `SWR` model object
#' @param mix a vector of mixing parameters (beta)
#' @param param a matrix with 2 columns representing one window per row. The first column contains location parameters delta, the second column contains the standard deviation sigma.
#' @export
createSWR <- function(list = NULL, param = NULL, mix = NULL){

  if(!is.null(list)){
    try(return(createSWR(param = list$param,
                  mix = list$mix)))
    stop("Error in createSWR: list is not consistent with SWR object definition")
  }
  else{
    if(!is.vector(mix) || !is.matrix(param) || length(mix) != nrow(param)){ # VERSION WITH INTERCEPT: nrow(param) + 1
      stop("Error in createSWR: provided parameters are not consistent")
    }

    mod <- list(param = param,
                mix = mix)
    class(mod) <- "SWR"

    return(mod)
  }
}
