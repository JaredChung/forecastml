# ========================================== #
# Project automatic forecast ML
# Jared Chung
# 23/08/2017
# file: utility
# ========================================== #

#' Utility functions
#'
#' @param data Time series object as an input
#' @param initialwindow Split the
#' @param horizon
#' @param fixed window
#'
#' @example
#'
#'
#'
#' @export


# Standard 80/20 train test split
train_test_data <- function(data) {

  if("ts" %in% class(data)) {
    data <- data.frame(date = as.Date(yearmon(time(data))), value=  as.matrix(data))
    train = data[1:floor(nrow(data)*0.8),]
    test = data[(floor(nrow(data)*0.8)+1):nrow(data),]
  } else {
    train = data[1:floor(nrow(data)*0.8),]
    test = data[(floor(nrow(data)*0.8)+1):nrow(data),]
  }
  return(list(train=train,test=test))

}


# Cross validation
cross_validation_data <- function(data,
                                  initialwindow = 0.7,
                                  horizon = 1,
                                  fixedwindow = TRUE) {

  if ("ts" %in% class(data)) {
    return(timeslice <- caret::createTimeSlices(1:length(data),
                                                initialWindow = length(data) * initialwindow,
                                                horizon = horizon,
                                                fixedWindow = fixedwindow))
  } else {
    return(timeslice <- caret::createTimeSlices(1:nrow(data),
                                                initialWindow = nrow(data) * initialwindow,
                                                horizon = horizon,
                                                fixedWindow = fixedwindow))
  }
  return(timeslice)
}


error_metric <- function(test, prediction) {


  error <- (xx - ff[1:n])[test]
  pe <- error / xx[test] * 100

  me <- mean(error, na.rm = TRUE)
  mse <- mean(error ^ 2, na.rm = TRUE)
  mae <- mean(abs(error), na.rm = TRUE)
  mape <- mean(abs(pe), na.rm = TRUE)
  mpe <- mean(pe, na.rm = TRUE)
  out <- c(me, sqrt(mse), mae, mpe, mape)
  names(out) <- c("ME", "RMSE", "MAE", "MPE", "MAPE")


}








