#' Summarizes an `SWR` model
#' @description A summary of an `SWR` model
#' @param object a `SWR` model object created using \link{train}
#' @param ... currently unused
#' @importFrom methods is
#' @importFrom knitr kable
#' @export
summary.SWR <- function(object,...){

  if(!is(object, "SWR")){
    stop("Wrong class of object")
  }

  k = nrow(object$param)

  cat(" SlidingWindowReg (SWR) model object with k = ", k, " windows\n\n");

  knitr::kable(
    data.frame(
      window = 1:k,
      delta = round(object$param[,1], 2),
      sigma = round(object$param[,2], 2),
      beta = round(object$mix, 2))
  )
}




