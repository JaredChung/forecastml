
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
library(data.table)
source("R/utils.R")
source("R/feature_extracter.R")

###################################
# H2o
####################################


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


  # To store data
  predictions <- matrix(nrow=length(timeslices),ncol = 4)
  results <- data.table()
  models <- data.table()

  # Convert TS object into dataframe
  data <- data.frame(list(date = as.Date(time(data)),
                          value = as.numeric(data)))

  external_regressor$date <- NULL
  external_regressor$value <- NULL

  data <- cbind(data,external_regressor)


  init <- h2o.init(strict_version_check = FALSE, nthreads = -1)

  # Cross validation time series
  for(i in 1:length(trainslices)) {


    #https://github.com/h2oai/h2o-tutorials/blob/master/h2o-open-tour-2016/chicago/grid-search-model-selection.R

    train_h2o <- h2o::as.h2o(data[trainslices[[i]],])

    test_h2o <- h2o::as.h2o(data[testslices[[i]],])

    y_index <- 'value'

    x_index <- setdiff(names(train_h2o), y_index)


    # Elastic Net
    alpha_opts = list(list(0), list(.25), list(.5), list(.75), list(1))
    hyper_parameters = list(alpha = alpha_opts)

    glm_h2o <- h2o::h2o.glm(x = x_index,
                            y = y_index,
                            training_frame = train_h2o,
                            validation_frame = test_h2o,
                            seed = seed,
                            family = "gaussian",
                            lambda_search = TRUE,
                            standardize = TRUE,
                            #hyper_params = hyper_parameters,
                            nfolds = 5)

    #
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


    # automl_models_h2o <- h2o.automl(x = x,
    #                             y = y,
    #                             training_frame = train_h2o,
    #                             validation_frame = test_h2o,
    #                             #leaderboard_frame = test_h2o,
    #                             max_runtime_secs = 3300,
    #                             stopping_metric = "AUTO")

    mlp_h2o <- h2o.deeplearning(
                          model_id="dl_model_first",
                          x = x_index,
                          y = y_index,
                          training_frame=train_h2o,
                          validation_frame=test_h2o,   ## validation dataset: used for scoring and early stopping
                          #activation="Rectifier",  ## default
                          #hidden=c(200,200),       ## default: 2 hidden layers with 200 neurons each
                          epochs=1,
                          variable_importances=T    ## not enabled by default

    )


    rmse_valid <- h2o.rmse(glm_h2o, valid=T)
    results[i, 1] <- h2o.rmse(glm_h2o, valid=T)
    results[]

    print(sprintf("--------- Time slice %s",i),sep="")
    print(sprintf("RMSE %s", rmse_valid))

    predictions[i,1] <-
    #results <- data.frame()
    #models <- data.frame()


  }


  h2o.shutdown(prompt=FALSE)


  return(rmse_valid)

}


# Testing

# Run

data <- a10

x_reg <- fit_feature_extracter(data, num_lag = 2, num_roll = 3)

result <- forecast_h2o(data,
                       external_regressor = x_reg)






#TESTING

cv_horizon <- 1
intitial_window <- 0.7

trainslices <- cross_validation_data(data,
                                     initialwindow = intitial_window,
                                     horizon = cv_horizon)$train
testslices <- cross_validation_data(data,
                                    initialwindow = intitial_window,
                                    horizon = cv_horizon)$test


h2o_result <- forecast_h2o(train = data[trainslices[[1]]],
                        test = data[testslices[[1]]])



# GBM hyperparamters
gbm_params1 <- list(learn_rate = c(0.01, 0.1),
                    max_depth = c(3, 5, 9),
                    sample_rate = c(0.8, 1.0),
                    col_sample_rate = c(0.2, 0.5, 1.0))

# Train and validate a grid of GBMs
gbm_grid1 <- h2o.grid("gbm", x = x, y = y,
                      grid_id = "gbm_grid1",
                      training_frame = train,
                      validation_frame = valid,
                      ntrees = 100,
                      seed = 1,
                      hyper_params = gbm_params1)

# Get the grid results, sorted by AUC
gbm_gridperf1 <- h2o.getGrid(grid_id = "gbm_grid1",
                             sort_by = "auc",
                             decreasing = TRUE)


###################################
# Caret
####################################


caret_forecast <- function {

}
















