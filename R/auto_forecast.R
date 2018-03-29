
# ========================================== #
# Project automatic forecast ML
# Jared Chung
# 23/08/2017
# file: Standard forecasting techniques
# ========================================== #

#' Forecast using traditional forecasting techniques
#'
#' @param data Time series object as an input
#' @param
#'
#' @example
#' @export


#######################################
# Main Forecast Function
#######################################

automatic_forecast <- function(data,
                               cv_horizon = 1,
                               verbose = FALSE,
                               intitial_window = 0.7,
                               external_regressor = NULL){



  # Split Dataset into cross validation slices
  trainslices <- cross_validation_data(data,
                                       initialwindow = intitial_window,
                                       horizon = cv_horizon)$train
  testslices <- cross_validation_data(data,
                                      initialwindow = intitial_window,
                                      horizon = cv_horizon)$test

  # Check if there are external regressors
  if(!is.null(external_regressor)) {

    trainslices_xreg <- cross_validation_data(external_regressor,
                                         initialwindow = intitial_window,
                                         horizon = cv_horizon)$train
    testslices_xreg <- cross_validation_data(external_regressor,
                                        initialwindow = intitial_window,
                                        horizon = cv_horizon)$test

  } else {

    trainslices_xreg <- NULL
    testslices_xreg <- NULL

  }

  # To store data
  predictions <- data.frame()
  results <- data.frame()
  models <- data.frame()


  # Convert the data input into a data frame
  data <- data.frame(list(date = as.Date(lubridate::date_decimal(as.numeric(time(data)))),
                           value = as.numeric(data)))



  # Cross validation time series
  for(i in 1:length(trainslices)) {

    if(verbose == TRUE) {
      print(sprintf("--------- Time slice %s",i))
      print(sprintf("--------- Train Length %s", length(trainslices[[i]])))
      print(sprintf("--------- Test Length %s",length(testslices[[i]])))
    }


    standard_forecast_result <- standard_forecast(train = data[trainslices[[i]],]$value,
                                                  test = data[testslices[[i]],]$value,
                                                  timeslice = i,
                                                  trainslices_xreg = trainslices_xreg,
                                                  testslices_xreg = testslices_xreg)


    h2o_forecast_result <- forecast_h2o(train = data[trainslices[[i]],],
                                        test = data[testslices[[i]],],
                                        seed = 42)

    shutdown <- h2o::h2o.shutdown(prompt=FALSE)

    #export the output
    if(nrow(predictions) == 0) {
         predictions <- as.data.frame(list(time = rownames(standard_forecast_result$ets$predictions),
                                           ets = standard_forecast_result$ets$predictions$`Point Forecast`,
                                           arima = standard_forecast_result$arima$predictions$`Point Forecast`,
                                           tbats = standard_forecast_result$tbats$predictions$`Point Forecast`,
                                           nnetar = standard_forecast_result$nnetar$predictions$`Point Forecast`,
                                           thetaf = standard_forecast_result$thetaf$predictions$`Point Forecast`))
    } else {
         predictions <- dplyr::bind_rows(predictions,as.data.frame(list(time = rownames(standard_forecast_result$ets$predictions),
                                                                 ets = standard_forecast_result$ets$predictions$`Point Forecast`,
                                                                 arima = standard_forecast_result$arima$predictions$`Point Forecast`,
                                                                 tbats = standard_forecast_result$tbats$predictions$`Point Forecast`,
                                                                 nnetar = standard_forecast_result$nnetar$predictions$`Point Forecast`,
                                                                 thetaf = standard_forecast_result$thetaf$predictions$`Point Forecast`)))
    }

    if(nrow(results) == 0) {
      results <- dplyr::bind_rows(standard_forecast_result$ets$result,
                                  standard_forecast_result$arima$result,
                                  standard_forecast_result$tbats$result,
                                  standard_forecast_result$nnetar$result,
                                  standard_forecast_result$thetaf$result)
    } else {
      results <- dplyr::bind_rows(results,
                                  standard_forecast_result$ets$result,
                                  standard_forecast_result$arima$result,
                                  standard_forecast_result$tbats$result,
                                  standard_forecast_result$nnetar$result,
                                  standard_forecast_result$thetaf$result)
    }

  }

  output <- list(results=results,
                 predictions=predictions)

  class(output) <- c(class(output),"forecastml")

  return(output)
}


# print
print.forecastml <- function(x,...) {
    print()
}

#------------------------------------
# Forecast Method
#------------------------------------

forecast.forecastml <- function(x) {


}



#------------------------------------
# Dashboard Method (Undercontruction)
#-----------------------------------



# # load practice data of pharmaceutical products

# library(fpp2) # data to test time series on
# data <- a10
#
# forecast_result <- automatic_forecast(data,
#                                       cv_horizon = 1,
#                                       verbose=TRUE)
#



# data <- data.frame(list(date = as.Date(lubridate::date_decimal(as.numeric(time(data)))),
#                         value = as.numeric(data)))



# train <- ts(data[1:42,]$value)
# test <- ts(data[43,]$value)

# #testing
# asdf <- ets(data)
#
# asdf2 <- forecast(asdf,h=1)
# asdf2 <- as.data.frame(asdf2)
#
# ffff <-as.data.frame(list(ets= asdf2$`Point Forecast`))
#
#
# asdf2[,1]



