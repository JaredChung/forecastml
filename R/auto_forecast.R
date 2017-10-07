
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




library(fpp2) # data to test time series on
library(forecast)
library(ggplot2)
library(caret)
library(tidyverse)
library(zoo)


# load test data of pharmaceutical products
data <- a10


preprocess_data <- function(data) {


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



standard_forecast <- function(data,start = ,frequency = 12) {

          #

          train <- ts(preprocess_data(data)$train$value,start = start , frequency = frequency)

          test <- tspreprocess_data(data)$test



          # Automatic Expoinential smoothing
          model <- forecast::ets(train)

          # Automatic Arima
          model2 <- forecast::auto.arima(train)

          # Automatic Exponential smoothing + ARMA + Box Cox transformations
          model3 <- forecast::tbats(train)

          # Basic single hidden layer neural network
          model4 <- forecast::nnetar(train)

          # Simple Exponential Smoothing
          model5 <- forecast::thetaf(train)


          model6 <-

          return()
}








