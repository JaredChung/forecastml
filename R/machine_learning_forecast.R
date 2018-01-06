
# ========================================== #
# Project automatic forecast ML
# Jared Chung
# 23/08/2017
# file: machine_learning_forecast
# ========================================== #

#' Forecast using standard forecasting
#'
#' @param data Time series object as an input
#' @param
#'
#' @example


#tempory import
library(tidyverse)
library(h2o)
library(fpp2)
source("R/utils.R")
source("R/feature_extracter.R")

###################################
# H2o
####################################

  #
  #   # print the number of rows used for the test_1 dataset
  #   print(paste(‘number of rows used in test set: ‘, dim(test_1)[1], sep=” “))
  #   print(paste(‘number of rows used in train set: ‘, dim(train_1)[1], sep=” “))
  #   # print the validation metrics
  #   rmse_valid <- h2o.rmse(gbm_model, valid=T)
  #   print(paste(‘your new rmse value on the validation set is: ‘, rmse_valid,‘ for fold #: ‘, counter, sep=“”))
  #
  #                # create new training frame
  #                train_1 <- h2o.rbind(train_1,test_1)
  #                print(paste(‘shape of new training dataset: ‘,dim(train_1)[1],sep=” “))
  #                counter <<- counter + 1
  # }

forecast_h2o <- function(data,
                         external_regressor = NULL,
                         seed = 42) {

  cv_horizon <- 1
  intitial_window <- 0.7

  trainslices <- cross_validation_data(data,
                                       initialwindow = intitial_window,
                                       horizon = cv_horizon)$train
  testslices <- cross_validation_data(data,
                                      initialwindow = intitial_window,
                                      horizon = cv_horizon)$test


  #Check if there are external regressors
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

  # Convert TS object into dataframe
  data <- data.frame(list(date = as.Date(time(data)),
                          value = as.numeric(data)))

  init <- h2o.init(strict_version_check = FALSE, nthreads = -1)

  # Cross validation time series
  for(i in 1:length(trainslices)) {


    train_h2o <- h2o::as.h2o(data[trainslices[[i]],])

    test_h2o <- h2o::as.h2o(data[testslices[[i]],])

    y_index <- 'value'

    x_index <- setdiff(names(train_h2o), y_index)


    glm_h2o <- h2o::h2o.glm(x = x_index,
                            y = y_index,
                            training_frame = train_h2o,
                            validation_frame = test_h2o,
                            seed = seed,
                            family = "gaussian")

    rf_h2o <- h2o::h2o.randomForest(x = x_index,
                            y = y_index,
                            training_frame = train_h2o,
                            validation_frame = test_h2o,
                            seed = seed,
                            ntrees = 200)

    gbm_h2o <- h2o::h2o.gbm(x = x_index,
                            y = y_index,
                            training_frame = train_h2o,
                            validation_frame = test_h2o,
                            seed = seed,
                            ntrees = 200
                              )



    rmse_valid <- h2o.rmse(glm_h2o, valid=T)
    print(sprintf("--------- Time slice %s",i),sep="")
    print(sprintf("RMSE %s", rmse_valid))
  }


  h2o.shutdown(prompt=FALSE)

}


# Testing

# Run

data <- a10

x_reg <- fit_feature_extracter(data, num_lag = 2, num_roll = 3)

result <- forecast_h2o(data,
                       external_regressor = x_reg)






#check

glm_h2o <- forecast_h2o(train = data[trainslices[[1]]],
                        test = data[testslices[[1]]])




###################################
# Caret
####################################


















