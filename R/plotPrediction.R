#' @title Plot prediction
#' @description plot the prediction and the reference
#' @examples
#' param <- cbind(
#'   delta = c(1,4),
#'   kernel_rad = c(1,3))
#' mixture <- rep(1,ncol(param) + 1)
#' pred <- predict(sampleWatershed$rain, mixture, param)
#' plot_prediction(pred, sampleWatershed$gauge, sampleWatershed$rain)
#' @import ggplot2
#' @export
plot_prediction <- function(prediction, reference, input_ts = NULL, scale = "year", years = NULL){
  d <- data.frame(t = 1:length(prediction),
                  gauge = c(prediction, reference),
                  type = rep(c("prediction", "reference"), each = length(prediction)),
                  category = "gauge",
                  year = rep(1 : (length(prediction) / 365), each = 365)[1:length(prediction)],
                  day_of_year = (1:length(prediction)) %% 365,
                  month = rep(1 : (length(prediction) / 30), each = 30)[1:length(prediction)],
                  day_of_month = (1:length(prediction)) %% 30,
                  week = rep(1 : (length(prediction) / 7), each = 7)[1:length(prediction)],
                  day_of_week = (1:length(prediction)) %% 7)
  if(!is.null(input_ts)){
    d <- rbind(d, data.frame(
      t = 1:length(input_ts),
      gauge = input_ts,
      type = "input",
      category = "rain",
      year = rep(1 : (length(input_ts) / 365), each = 365)[1:length(input_ts)],
      day_of_year = (1:length(input_ts)) %% 365,
      month = rep(1 : (length(input_ts) / 30), each = 30)[1:length(input_ts)],
      day_of_month = (1:length(input_ts)) %% 30,
      week = rep(1 : (length(input_ts) / 7), each = 7)[1:length(input_ts)],
      day_of_week = (1:length(input_ts)) %% 7)
    )
  }
  d$type <- factor(d$type, levels = c("prediction", "reference", "input"))
  d <- na.omit(d)
  if(scale == "year"){
    if(is.null(years)){
      max_year <- max(subset(d, type == "prediction")$year)
      d <- subset(d, year <= 3 | (year > (max_year - 3)))
    }
    else{
      d <- subset(d, year %in% years)
    }
    p <- ggplot(d, aes(x = day_of_year, y = gauge, col = type, group_by = type)) +
      facet_grid(category~year, scales = "free_y")+
      geom_line(size = 0.65)+
      theme_bw()+
      theme(text = element_text(size = 25),
            axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
      xlab("day of hydr. year") +
      ylab("value") +
      scale_x_continuous(breaks=c(1, 100, 200, 300))
  }
  else if(scale == "month"){
    max_month <- max(subset(d, type == "prediction")$month) - 3
    d <- subset(d, month <= 2 | (month > (max_month - 3) & month < max_month))
    p <- ggplot(d, aes(x = day_of_month, y = gauge, col = type, group_by = type)) +
      facet_grid(category~month, scales = "free_y")+
      geom_line(size = 0.8) +
      geom_point()+
      theme_bw()+
      theme(text = element_text(size = 25))
  }
  else if(scale == "week"){
    max_week <- max(subset(d, type == "prediction")$week) - 3
    d <- subset(d, week <= 3 | (week > (max_week - 4) & week < max_week))
    p <- ggplot(d, aes(x = day_of_week, y = gauge, col = type, group_by = type)) +
      facet_grid(category~week, scales = "free_y")+
      geom_line(size = 0.8) +
      geom_point()+
      theme_bw()+
      theme(text = element_text(size = 25))
  }
  p
}
