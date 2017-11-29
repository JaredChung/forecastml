
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
source(feature_extracter)
#library(R6) convert to R6 Class later ....


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


# Access forecast package
run_forecast <- function(train, test,FUN, name, timeslice,  train_regressor= NULL, test_regressor=NULL,...) {

     # check if there is external regressors
     if(is.null(train_regressor)) {
        model <- FUN(train, ...)
        predictions <-forecast::forecast(model, h = length(test))
     } else {
        model <- FUN(train, xreg = train_regressor , ...)
        predictions <-forecast::forecast(model, h = length(test), xreg = test_regressor)
     }

      result <- forecast::accuracy(predictions, test) %>%
                                      as.data.frame() %>%
                                      rownames_to_column() %>%
                                      mutate(model = name,
                                             timeslice = timeslice)

      return(list(predictions = as.data.frame(predictions), result = result, model = model ))
}


automatic_forecast <- function(data, cv_horizon = 1, verbose = FALSE, external_regressor = NULL){

      # Split Dataset into cross validation slices
      trainslices <- cross_validation_data(data,
                                           initialwindow = 0.7,
                                           horizon = cv_horizon)$train
      testslices <- cross_validation_data(data,
                                          initialwindow = 0.7,
                                          horizon = cv_horizon)$test

      # Check if there are external regressors
      if(!is.null(external_regressor)) {

          trainslices_xreg <- cross_validation_data(external_regressor,
                                               initialwindow = 0.7,
                                               horizon = cv_horizon)$train
          testslices_xreg <- cross_validation_data(external_regressor,
                                              initialwindow = 0.7,
                                              horizon = cv_horizon)$test

      } else {

          trainslices_xreg <- NULL
          testslices_xreg <- NULL

      }

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
                              lambda = forecast::BoxCox.lambda(data[trainslices[[i]]]),
                              train_regressor = trainslices_xreg,
                              test_regressor = testslices_xreg)

          thetaf <- run_forecast(train = data[trainslices[[i]]],
                              test = data[testslices[[i]]],
                              FUN = forecast::thetaf,
                              name = 'thetaf',
                              timeslice = i)


          #export the output
          if(nrow(predictions) == 0) {
               predictions <- as.data.frame(list(time = rownames(ets$predictions),
                                                 ets = ets$predictions$`Point Forecast`,
                                                 arima = arima$predictions$`Point Forecast`,
                                                 tbats = tbats$predictions$`Point Forecast`,
                                                 nnetar = nnetar$predictions$`Point Forecast`,
                                                 thetaf = thetaf$predictions$`Point Forecast`))
          } else {
               predictions <- bind_rows(predictions,as.data.frame(list(time = rownames(ets$predictions),
                                                                       ets = ets$predictions$`Point Forecast`,
                                                                       arima = arima$predictions$`Point Forecast`,
                                                                       tbats = tbats$predictions$`Point Forecast`,
                                                                       nnetar = nnetar$predictions$`Point Forecast`,
                                                                       thetaf = thetaf$predictions$`Point Forecast`)))
          }

          if(nrow(results) == 0) {
               results <- bind_rows(ets$result,arima$result, tbats$result, nnetar$result, thetaf$result)
          } else {
               results <- bind_rows(results,ets$result,arima$result, tbats$result, nnetar$result, thetaf$result)
          }

      }

      output <- list(results=results,predictions=predictions)

      class(output) <- c(class(output),"forecastml")

      return(output)
}


#------------------------------------
# Plot Method
#---------------------------------


plot <- function(forecast) {

      UseMethod("plot",forecast)

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

#------------------------------------
# Dashboard Method (Undercontruction)
#-----------------------------------



# load practice data of pharmaceutical products
data <- a10



forecast_result <- automatic_forecast(data,cv_horizon = 1,verbose=TRUE)




#testing
asdf <- ets(data)

asdf2 <- forecast(asdf,h=1)
asdf2 <- as.data.frame(asdf2)

ffff <-as.data.frame(list(ets= asdf2$`Point Forecast`))


asdf2[,1]



