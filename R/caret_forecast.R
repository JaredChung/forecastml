# ========================================== #
# Project automatic forecast ML
# Jared Chung
# 23/08/2017
# file: machine_learning_forecast
# ========================================== #

#' Forecast using caret based Machine learning techniques
#'
#' @param train Takes a TS object as an argument
#' @param test
#' @param
#' @param
#' @param
#' @param
#'
#' @example

#' @export




###################################
# Caret
####################################


caret_forecast <- function (train,
                            test){



      # Elastic net
      lambda_grid <- 10 ^ seq(2, -2, length = 100)
      alpha_grid <- seq(0, 1, length = 10)

      glmnet_grid <- expand.grid(lambda = lambda_grid,
                                 alpha = alpha_grid)

      glmnet_model <- train(value ~ .,
                            data = train,
                            method = "glmnet",
                            tuneGrid = glmnet_grid,
                            trControl = )


      # Random Forest


      # Gradient Boosted Machine






}






library(fpp2)

data <- a10

trainslices <- cross_validation_data(data)$train
testslices <- cross_validation_data(data)$test


fit <- auto.arima(data[trainslices[[1]]])

predict <- forecast(fit,h = 1)

error_metric(data[testslices[[1]]],as.numeric(predict$mean))

forecast::accuracy(predict,data[testslices[[1]]])





