# ========================================== #
# Project automatic forecast ML
# Jared Chung
# 23/08/2017
# file: Standard forecasting techniques
# ========================================== #

#' Forecast using standard forecasting
#'
#' @param data Time series object as an input
#' @param
#'
#' @example



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
                                  horizon = 12,
                                  fixedWindow = TRUE) {

  if ("ts" %in% class(data)) {
    return(timeslice <- caret::createTimeSlices(1:length(data),
                                                initialWindow = length(data) * initialwindow,
                                                horizon = horizon,
                                                fixedWindow = fixedWindow))
  } else {
    return(timeslice <- caret::createTimeSlices(1:nrow(data),
                                                initialWindow = nrow(data) * initialwindow,
                                                horizon = horizon,
                                                fixedWindow = fixedWindow))
  }
  return(timeslice)
}




