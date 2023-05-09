#' @title Plot prediction
#' @description plot the prediction and the reference
#' @inheritParams eval_all
#' @inheritParams trainSWR
#' @param scale a String indicating which scale should be used for plotting; options are "year", "month" or "week"; default is "year"
#' @param years a vector of years to be included in the plot; if NULL, the first 3 and last 3 years will be used
#' @param ... currently unused
#' @examples
#' param <- cbind(
#'   delta = c(0, 10),
#'   sigma = c(2, 3))
#' mix <- rep(1, ncol(param))
#' mod <- createSWR(param = param, mix = mix)
#' pred <- predict(mod, newdata = sampleWatershed$rain)
#'
#' plot_prediction(pred,
#'                 prediction = sampleWatershed$gauge,
#'                 reference = sampleWatershed$rain)
#' plot(mod,
#'      type = "prediction",
#'      newdata = sampleWatershed$rain,
#'      reference = sampleWatershed$gauge)
#' @import ggplot2
#' @importFrom stats na.omit
#' @export
plot_prediction <- function(prediction, reference, ts_input = NULL, scale = "year", years = NULL, ...){

  # create data.frame
  d <- data.frame(t = 1:length(prediction),
                  gauge = c(prediction, reference),
                  type = rep(c("prediction", "ground truth"), each = length(prediction)),
                  category = "gauge",
                  year = rep(1 : (length(prediction) / 365), each = 365)[1:length(prediction)],
                  day_of_year = (1:length(prediction)) %% 365,
                  month = rep(1 : (length(prediction) / 30), each = 30)[1:length(prediction)],
                  day_of_month = (1:length(prediction)) %% 30,
                  week = rep(1 : (length(prediction) / 7), each = 7)[1:length(prediction)],
                  day_of_week = (1:length(prediction)) %% 7)

  # append input time series, if provided
  if(!is.null(ts_input)){
    d <- rbind(d, data.frame(
      t = 1:length(ts_input),
      gauge = ts_input,
      type = "input",
      category = "rain",
      year = rep(1 : (length(ts_input) / 365), each = 365)[1:length(ts_input)],
      day_of_year = (1:length(ts_input)) %% 365,
      month = rep(1 : (length(ts_input) / 30), each = 30)[1:length(ts_input)],
      day_of_month = (1:length(ts_input)) %% 30,
      week = rep(1 : (length(ts_input) / 7), each = 7)[1:length(ts_input)],
      day_of_week = (1:length(ts_input)) %% 7)
    )
  }

  # convert to 1-indexing and convert type to factor
  d$day_of_year[d$day_of_year == 0] <- 365
  d$day_of_month[d$day_of_month == 0] <- 30
  d$day_of_week[d$day_of_week == 0] <- 7
  d$type <- factor(d$type, levels = c("ground truth", "prediction", "input"))

  # omit NA values
  d <- na.omit(d)

  # plot
  if(scale == "year"){

    # restrict years, if provided, otherwise use default
    if(is.null(years)){
      max_year <- max(subset(d, type == "prediction")$year)
      d <- subset(d, year <= 3 | (year > (max_year - 3)))
    }
    else{
      d <- subset(d, year %in% years)
    }

    # avoid package compilation warning
    day_of_year <- gauge <- type <- year <- NULL

    p <- ggplot(d, aes(x = day_of_year, y = gauge, col = type, group_by = type)) +
      facet_grid(type~year, scales = "free_y")+
      xlab("day of hydr. year") +
      scale_x_continuous(breaks=c(50, 150, 250, 350))

  } else if(scale == "month"){

    # restrict months
    max_month <- max(subset(d, type == "prediction")$month) - 3
    d <- subset(d, month <= 3 | (month > (max_month - 4) & month < max_month))

    # avoid package compilation warning
    day_of_month <- gauge <- type <- month <- NULL

    p <- ggplot(d, aes(x = day_of_month, y = gauge, col = type, group_by = type)) +
      facet_grid(category~month, scales = "free_y") +
      xlab("day of month") +
      scale_x_continuous(breaks=c(5, 15, 25))

  } else if(scale == "week"){

    # restrict weeks
    max_week <- max(subset(d, type == "prediction")$week) - 3
    d <- subset(d, week <= 3 | (week > (max_week - 4) & week < max_week))

    # avoid package compilation warning
    day_of_week <- gauge <- type <- week <- NULL

    p <- ggplot(d, aes(x = day_of_week, y = gauge, col = type, group_by = type)) +
      facet_grid(category~week, scales = "free_y") +
      xlab("day of week") +
      scale_x_continuous(breaks=c(2, 4, 6))
  }
  else{
    stop("Error in plot: unknown scale provided")
  }

  p <- p +
    geom_point() +
    geom_line(size = 0.8) +
    theme_bw() +
    theme(text = element_text(size = 25),
          axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
          legend.position = "top") +
    ylab("value")

  return(p)
}
