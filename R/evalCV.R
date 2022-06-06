#' @title Evaluate prediction
#' @param list a list containing multiple trained SlidingWindowReg models
#' @import dplyr
#' @importFrom stats var
#' @export

evalCV <- function(list){
  if(is.null(dim(list)) | length(dim(list)) != 3){
    stop("Error: evaluating cross-validation result requires an array with 3 dimensions (runs, cv-folds, lambda)")
  }

  # initialize dimnames
  runs <- dim(list)[1]
  cv_folds <- dim(list)[2]
  lambda <- dim(list)[3]

  # retrieve metrics from list
  df <- data.frame(type = c(), metric = c(), lambda = c(), cv_fold = c(), run = c(), value = c())
  for(i in 1:runs){
    for(j in 1:cv_folds){
      for(k in 1:lambda){
        train_perf <- unlist(list[i,j,k][[1]]$train_metrics)
        test_perf <- unlist(list[i,j,k][[1]]$test_metrics)
        df <- rbind(df,
                    data.frame(type = "train",
                      metric = c(names(train_perf), "num_windows"),
                      lambda = dimnames(list)[[3]][k],
                      cv_fold = dimnames(list)[[2]][j],
                      run = dimnames(list)[[1]][i],
                      value = c(train_perf, nrow(list[i,j,k][[1]]$param))
                    ),
                    data.frame(type = "test",
                      metric = names(test_perf),
                      lambda = dimnames(list)[[3]][k],
                      cv_fold = dimnames(list)[[2]][j],
                      run = dimnames(list)[[1]][i],
                      value = test_perf
                    )
              )
      }
    }
  }

  # avoid R check errors
  value <- type <- metric <- cv_fold <- NULL

  # compute summaries
  summary <- df %>% group_by(type, metric, lambda, cv_fold) %>% summarize(mean = mean(value, na.rm = TRUE),
                                                                          var = var(value, na.rm = TRUE),
                                                                          min = min(value, na.rm = TRUE),
                                                                          max = max(value, na.rm = TRUE),
                                                                          which_min = which.min(value),
                                                                          which_max = which.max(value))
  lambda_summary <- df %>% group_by(type, metric, lambda) %>% summarize(mean = mean(value, na.rm = TRUE),
                                                                        var = var(value, na.rm = TRUE),
                                                                        min = min(value, na.rm = TRUE),
                                                                        max = max(value, na.rm = TRUE))

  best_rmse_runs <- df %>% group_by(type, lambda, cv_fold) %>% filter(metric == "rmse") %>% filter(value == min(value))

  return(list(
    full = df,
    summary = summary,
    lambda_summary = lambda_summary,
    best_rmse_runs = best_rmse_runs)
  )
}
