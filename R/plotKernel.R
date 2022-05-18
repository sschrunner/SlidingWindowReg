#' @title Return kernel
#' @description return cumulated kernel
#' @export
get_kernel <- function(param, mixture = NULL, type = "single"){
  kernel_length <- max(param[,1] + param[,2] + 1)
  comb_kernel <- matrix(rep(0, kernel_length), nrow = nrow(param), ncol = kernel_length)

  for(i in 1:nrow(param)){
    delta = param[i,1]
    kernel_rad = param[i,2]

    comb_kernel[i,(kernel_length - delta - kernel_rad) : min(kernel_length - delta + kernel_rad, kernel_length)] <-
      comb_kernel[i,(kernel_length - delta - kernel_rad) : min(kernel_length - delta + kernel_rad, kernel_length)] +
      gauss_kernel(kernel_rad, truncation = max(c(delta - kernel_rad, 0)))[1:min(2 * kernel_rad + 1,kernel_rad + delta + 1)]
  }
  rownames(comb_kernel) <- paste0("kernel", 1:nrow(param))

  if(type == "single" | type == "single_only"){
    return(comb_kernel)
  }
  else if(type == "single_weighted" | type == "single_weighted_only"){
    return(mixture[-1] * comb_kernel)
  }
  else if(type == "combined"){
    return(colSums(comb_kernel))
  }
  else if(type == "combined_weighted"){
    if(is.null(mixture)){
      stop("Mixture must be specified")
    }
    return(as.vector(t(mixture[-1]) %*% comb_kernel))
  }
}

#' @title Plot kernel
#' @description plot cumulated kernel
#' @examples
#' param <- cbind(
#'   delta = c(1,4),
#'   kernel_rad = c(1,3))
#' mixture <- rep(1,ncol(param) + 1)
#' plot_kernel(param = param, mixture = mixture)
#' @import ggplot2
#' @export
plot_kernel <- function(param, mixture = NULL, type = "single"){
  comb_kernel <- get_kernel(param, mixture, type)
  if(type %in% c("single", "single_weighted")){
    d <- data.frame(t = -(ncol(comb_kernel)-1) : 0,
                    x = c(colSums(comb_kernel), as.vector(t(comb_kernel))),
                    component = ordered(rep(c("all",1:nrow(comb_kernel)), each = ncol(comb_kernel))),
                    type = factor(c(rep("combined", ncol(comb_kernel)), rep("component", length(comb_kernel))), levels = c("component","combined")))
    ggplot(subset(d, x > 0),
           aes(x = t, y = x, color = component, group_by = component)) +
      geom_line(size = 1)+
      geom_point() +
      scale_color_brewer(palette = "Dark2") +
      facet_grid(type~.)+
      theme_bw()+
      theme(text = element_text(size = 25))
  }
  else if(type %in% c("single_only", "single_weighted_only")){
    d <- data.frame(t = -(ncol(comb_kernel)-1) : 0,
                    x = as.vector(t(comb_kernel)),
                    component = ordered(rep(1:nrow(comb_kernel), each = ncol(comb_kernel))))
    ggplot(subset(d, x > 0),
           aes(x = t, y = x, color = component, group_by = component)) +
      geom_line(size = 1)+
      geom_point() +
      scale_color_brewer(palette = "Dark2")+
      theme_bw()+
      theme(text = element_text(size = 25))
  }
  else{
    ggplot(data.frame(t = -(ncol(comb_kernel)-1) : 0, x = comb_kernel),
           aes(x = t, y = x)) +
      geom_line(size = 1)+
      theme_bw()+
      theme(text = element_text(size = 25))
  }
}
