#' Summarizes an `SWR` model
#' @description A summary of an `SWR` model
#' @describeIn summary.SWR Summarizes the `SWR` model
#' @param object an `SWR` model object created using \link{trainSWR}
#' @param ... currently unused
#' @importFrom methods is
#' @importFrom knitr kable
#' @export
summary.SWR <- function(object,...){

  if(!is(object, "SWR")){
    stop("Wrong class of object")
  }

  k = nrow(object$param)

  cat("SlidingWindowReg (SWR) model object with k =", k, "windows")

  print(
    knitr::kable(
      data.frame(
        window = 1:k,
        delta = round(object$param[,"delta"], 2),
        sigma = round(object$param[,"sigma"], 2),
        beta = round(object$mix, 2))
    )
  )

  cat("\n")
}

#' @describeIn summary.SWR Returns the number of windows of the `SWR` model
#' @param x an `SWR` model object created using \link{trainSWR}
#' @importFrom methods is
#' @export
dim.SWR <- function(x){

  if(!is(x, "SWR")){
    stop("Wrong class of object")
  }

  return(nrow(x$param))
}




