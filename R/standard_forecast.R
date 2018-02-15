# ========================================== #
# Project automatic forecast ML
# Jared Chung
# 23/08/2017
# file: machine_learning_forecast
# ========================================== #

#' Forecast using traditional forecasting techniques
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


###########################################
#
###########################################



# Access forecast package
fit_forecast <- function(train, test,FUN, name, timeslice,  train_regressor= NULL, test_regressor=NULL,...) {

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

standard_forecast <- function(train,
                              test) {

  ets <- fit_forecast(train = train,
                      test = test,
                      FUN = forecast::ets,
                      name = 'ets',
                      timeslice = i,
                      lambda = forecast::BoxCox.lambda(data[trainslices[[i]]]))

  arima <- fit_forecast(train = train,
                        test = test,
                        FUN = forecast::auto.arima,
                        name = 'arima',
                        timeslice = i,
                        lambda = forecast::BoxCox.lambda(data[trainslices[[i]]]))

  tbats <- fit_forecast(train = train,
                        test = test,
                        FUN = forecast::tbats,
                        name = 'tbats',
                        timeslice = i)

  nnetar <- fit_forecast(train = train,
                         test = test,
                         FUN = forecast::nnetar,
                         name = 'nnetar',
                         timeslice = i,
                         lambda = forecast::BoxCox.lambda(data[trainslices[[i]]]),
                         train_regressor = trainslices_xreg,
                         test_regressor = testslices_xreg)

  thetaf <- fit_forecast(train = train,
                         test = test,
                         FUN = forecast::thetaf,
                         name = 'thetaf',
                         timeslice = i)

  return(list(ets = ets,
              arima = arima,
              tbats = tbats,
              nnetar = nnetar,
              thetaf = thetaf))
}

