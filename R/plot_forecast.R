
# ========================================== #
# Project automatic forecast ML
# Jared Chung
# 23/08/2017
# file: feature_extracter
# ========================================== #

#' Plot the forecast results
#'
#' @param data Time series object as an input
#'
#' @example
#'
#'
#'
#' @export
#'



#------------------------------------
# Plot Method
#---------------------------------


plot <- function(forecast) {

  UseMethod("plot")

}

plot.default <- function(forecast) {

  message("Error unable to deal with this object")
  return(forecast)

}

plot.forecastml <- function(forecast) {


  plot <- forecast$results %>% filter(rowname == "Test set") %>%
    select(-c(ACF1,ACF1,timeslice)) %>%
    group_by(model) %>%
    summarise(avg_ME = mean(ME),
              std_ME = sd(ME),
              avg_RMSE = mean(RMSE),
              std_RMSE = sd(RMSE),
              avg_MAE = mean(MAE),
              std_MAE = sd(MAE),
              avg_MPE = mean(MPE),
              std_MPE = sd(MPE),
              avg_MAPE = mean(MAPE),
              std_MAPE = sd(MAPE),
              avg_MASE = mean(MASE),
              std_MASE = sd(MASE)
    ) %>%
    ggplot(aes(model,avg_RMSE,fill=model)) + geom_col()


  #forecast_result$predictions %>% gather(model,amount,-time ) %>% View()

  return(plot)


}
