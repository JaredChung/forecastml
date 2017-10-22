
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
library(lubridate)
library(parallel)
library(R6)


# load practice data of pharmaceutical products
data <- a10


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
      }else {
          return(timeslice <- caret::createTimeSlices(1:nrow(data),
                                               initialWindow = nrow(data) * initialwindow,
                                               horizon = horizon,
                                               fixedWindow = fixedWindow))
      }
      return(timeslice)
}


# Access forecast package
run_forecast <- function(train, test,FUN, name, timeslice ,...) {

      model <- FUN(train, ...)

      predictions <-forecast::forecast(model, h = length(test))

      result <- forecast::accuracy(predictions, test) %>%
                                      as.data.frame() %>%
                                      rownames_to_column() %>%
                                      mutate(model = name,
                                             timeslice = timeslice)

      return(list(predictions = as.data.frame(predictions), result = result, model = model ))
}


automatic_forecast <- function(data, cv_horizon = 6, verbose = FALSE){

      #

      trainslices <- cross_validation_data(data,
                                           initialwindow = 0.7,
                                           horizon = cv_horizon)$train
      testslices <- cross_validation_data(data,
                                          initialwindow = 0.7,
                                          horizon = cv_horizon)$test

      predictions <- data.frame()
      results <- data.frame()
      models <- data.frame()


      # Cross validation time series
      for(i in 1:length(trainslices)) {

          if(verbose == TRUE) {
            print(sprintf("--------- Time slice %s",i))
            print(sprintf("--------- Train Length %s", length(trainslices[[i]])))
            print(sprintf("--------- Test Length %s",length(testslices[[i]])))
          }

          ets <- run_forecast(train = data[trainslices[[i]]],
                              test = data[testslices[[i]]],
                              FUN = forecast::ets,
                              name = 'ets',
                              timeslice = i,
                              lambda = forecast::BoxCox.lambda(data[trainslices[[i]]]))

          arima <- run_forecast(train = data[trainslices[[i]]],
                              test = data[testslices[[i]]],
                              FUN = forecast::auto.arima,
                              name = 'arima',
                              timeslice = i,
                              lambda = forecast::BoxCox.lambda(data[trainslices[[i]]]))

          tbats <- run_forecast(train = data[trainslices[[i]]],
                              test = data[testslices[[i]]],
                              FUN = forecast::tbats,
                              name = 'tbats',
                              timeslice = i)

          nnetar <- run_forecast(train = data[trainslices[[i]]],
                              test = data[testslices[[i]]],
                              FUN = forecast::nnetar,
                              name = 'nnetar',
                              timeslice = i,
                              lambda = forecast::BoxCox.lambda(data[trainslices[[i]]]))

          thetaf <- run_forecast(train = data[trainslices[[i]]],
                              test = data[testslices[[i]]],
                              FUN = forecast::thetaf,
                              name = 'thetaf',
                              timeslice = i)



          if(nrow(predictions) == 0) {
               predictions <- bind_rows(ets$predictions,arima$predictions, tbats$predictions, nnetar$predictions, thetaf$predictions)
          } else {
               predictions <- bind_rows(predictions,ets$predictions,arima$predictions, tbats$predictions, nnetar$predictions, thetaf$predictions)
          }

          if(nrow(results) == 0) {
               results <- bind_rows(ets$result,arima$result, tbats$result, nnetar$result, thetaf$result)
          } else {
               results <- bind_rows(results,ets$result,arima$result, tbats$result, nnetar$result, thetaf$result)
          }

      }

      return(list(results=results,predictions=predictions))
}


forecast_plots <- function(results) {


      forecast_result$results %>% filter(rowname == "Test set") %>%
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


      forecast_result$predictions


}



forecast_result <- automatic_forecast(data)










