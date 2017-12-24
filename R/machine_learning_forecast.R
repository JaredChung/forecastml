
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

forecast_h2o <- function(train,
                         test,
                         target = 'none',
                         num_thread = -1) {

    h2o.init(strict_version_check = FALSE, nthreads = num_thread)

    if(is.ts(train)){
        train <- as.dataframe(date = as.data.frame(list(date = as.Date(time(train)))), value = as.data.frame(train))
        test <- as.dataframe(date = as.data.frame(list(date = as.Date(time(test)))), value = as.data.frame(test))
    }

    # convert to an h2o dataframe
    train_h2o <- as.h2o(train)

    test_h2o <- as.h2o(test)

    # specify the features and the target column
    features <- data[,-target]

    # split dataset in ~half which if you round up is 77 rows (train on the first half of the dataset)
    train_1 <- air_h2o[1:ceiling(dim(air_h2o)[1]/2),]
    # calculate 14 days in unix time: one day is 86400 seconds in unix time (aka posix time, epoch time)
    # use this variable to iterate forward 12 days
    add_14_days <- 86400*14
    # initialize a counter for the while loop so you can keep track of which fold corresponds to which rmse
    counter <- 0
    # iterate over the process of testing on the next two weeks
    # combine the train_1 and test_1 datasets after each loop
    while (dim(train_1)[1] < dim(air_h2o)[1]){
      # get new dataset two weeks out
      # take the last date in Date column and add 14 days to i
      new_end_date <- train_1[nrow(train_1),]$Date + add_14_days
      last_current_date <- train_1[nrow(train_1),]$Date

      # slice with a boolean mask
      mask <- air_h2o[,“Date”] > last_current_date
      temp_df <- air_h2o[mask,]
      mask_2 <- air_h2o[,“Date”] < new_end_date

      # multiply the mask dataframes to get the intersection
      final_mask <- mask*mask_2
      test_1 <- air_h2o[final_mask,]

      # build a basic gbm using the default parameters
      gbm_model <- h2o.gbm(x = features, y = target, training_frame = train_1, validation_frame = test_1, seed = 1234)

      # print the number of rows used for the test_1 dataset
      print(paste(‘number of rows used in test set: ‘, dim(test_1)[1], sep=” “))
      print(paste(‘number of rows used in train set: ‘, dim(train_1)[1], sep=” “))
      # print the validation metrics
      rmse_valid <- h2o.rmse(gbm_model, valid=T)
      print(paste(‘your new rmse value on the validation set is: ‘, rmse_valid,‘ for fold #: ‘, counter, sep=“”))

                   # create new training frame
                   train_1 <- h2o.rbind(train_1,test_1)
                   print(paste(‘shape of new training dataset: ‘,dim(train_1)[1],sep=” “))
                   counter <<- counter + 1
    }


###################################
# Caret
####################################







# Testing

data <- a10


cv_horizon <- 1
intitial_window <- 0.7

trainslices <- cross_validation_data(data,
                                     initialwindow = intitial_window,
                                     horizon = cv_horizon)$train
testslices <- cross_validation_data(data,
                                    initialwindow = intitial_window,
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

# To store data
predictions <- data.frame()
results <- data.frame()
models <- data.frame()


# Cross validation time series
for(i in 1:length(trainslices)) {

















