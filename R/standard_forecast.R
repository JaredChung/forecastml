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
fit_forecast <- function(train,
                         test,
                         FUN,
                         name,
                         timeslice,
                         train_regressor = NULL,
                         test_regressor = NULL,
                         ...) {

  # check if there is external regressors
  if(is.null(train_regressor)) {
    model <- FUN(train, ...)
    predictions <-forecast::forecast(model, h = length(test))
  } else {
    model <- FUN(train, xreg = train_regressor, ...)
    predictions <-forecast::forecast(model, h = length(test), xreg = test_regressor)
  }

  result <- forecast::accuracy(predictions, test) %>%
    as.data.frame() %>%
    tibble::rownames_to_column() %>%
    dplyr::mutate(model = name,
           timeslice = timeslice)

  return(list(predictions = as.data.frame(predictions), result = result, model = model ))
}

standard_forecast <- function(train,
                              test,
                              timeslice,
                              trainslices_xreg,
                              testslices_xreg) {

  ets <- fit_forecast(train = train,
                      test = test,
                      FUN = forecast::ets,
                      name = 'ets',
                      timeslice = timeslice,
                      lambda = forecast::BoxCox.lambda(train))

  arima <- fit_forecast(train = train,
                        test = test,
                        FUN = forecast::auto.arima,
                        name = 'arima',
                        timeslice = timeslice,
                        lambda = forecast::BoxCox.lambda(train))

  tbats <- fit_forecast(train = train,
                        test = test,
                        FUN = forecast::tbats,
                        name = 'tbats',
                        timeslice = timeslice)

  nnetar <- fit_forecast(train = train,
                         test = test,
                         FUN = forecast::nnetar,
                         name = 'nnetar',
                         timeslice = timeslice,
                         lambda = forecast::BoxCox.lambda(train),
                         train_regressor = trainslices_xreg,
                         test_regressor = testslices_xreg)

  thetaf <- fit_forecast(train = train,
                         test = test,
                         FUN = forecast::thetaf,
                         name = 'thetaf',
                         timeslice = timeslice)

  return(list(ets = ets,
              arima = arima,
              tbats = tbats,
              nnetar = nnetar,
              thetaf = thetaf))
}

