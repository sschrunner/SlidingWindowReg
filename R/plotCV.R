#' @title Evaluate and plot cross-validation result
#' @description Evaluate and plot the results from a list of models, consisting of multiple runs, cross-validation folds, and lambda parameters
#' @param list a list containing multiple trained SlidingWindowReg models
#' @examples
#' set.seed(42)
#' models <- train(sampleWatershed$rain,
#'                sampleWatershed$gauge,
#'                iter = 2,
#'                cv_fold = 2,
#'                runs = 2,
#'                lambda = c(0.01,0.1),
#'                parallel = FALSE)
#' eval_CV(models)
#' plot_CV(models)
#' @import dplyr
#' @import ggplot2
#' @importFrom stats var
#' @export

eval_CV <- function(list){
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
  value <- type <- metric <- cv_fold <- train_rmse <- test_rmse <- train_value <- test_value <- mean_train <- NULL

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

  # determine runs with best RMSE on training data and create list_best
  best_rmse_runs <- df %>% group_by(type, lambda, cv_fold) %>% filter(metric == "rmse") %>% filter(value == min(value)) %>%
    distinct(type, lambda, cv_fold, value, metric, .keep_all = TRUE) %>% arrange(lambda, cv_fold)
  best_rmse_run_inds <- subset(best_rmse_runs, type == "train")[, c("run", "cv_fold", "lambda")]
  list_red <- list[as.matrix(best_rmse_run_inds)]
  dim(list_red) <- dim(list)[c(2,3)]
  dimnames(list_red) <- dimnames(list)[c(2,3)]

  # # determine best lambda
  best_rmse_runs %>%
    mutate(train_rmse = value * (type == "train"), test_rmse = value * (type == "test")) %>%
    group_by(lambda, cv_fold) %>%
    summarize(train_value = max(train_rmse), test_value = max(test_rmse)) -> best_rmse_runs_train
  best_rmse_runs_train %>%
    group_by(lambda) %>%
    summarize(mean_train = mean(train_value),
              mean_test = mean(test_value),
              var_train = var(train_value),
              var_test = var(test_value)) -> best_runs_mean_rmse
  best_runs_mean_rmse %>%
    filter(mean_train == min(mean_train)) -> best_lambda



  return(list(
    full = df,
    summary = summary,
    lambda_summary = lambda_summary,
    best_rmse_runs = best_rmse_runs,
    list_best = list_red,
    best_runs_mean_rmse = best_runs_mean_rmse,
    best_lambda = best_lambda,
    best_model = list_red[,best_lambda$lambda]
    )
  )
}

#' @title Plot cross-validation result
#' @rdname eval_CV
#' @import ggplot2
#' @export
plot_CV <- function(list){

  # avoid R check errors
  value <- lambda <- metric <- type <-  NULL

  # obtain best_rmse_runs
  best_rmse_runs <- eval_CV(list)$best_rmse_runs
  best_rmse_runs$lambda <- sapply(best_rmse_runs$lambda, function(x){strsplit(x, split = "=")[[1]][2]})

  p <- ggplot(best_rmse_runs, aes(x = value,
                                  y = lambda,
                                  fill = type,
                                  group_by = type)) +
    geom_boxplot() +
    theme_bw() +
    theme(text = element_text(size = 25),
          legend.position = "top")

  return(p)

}
