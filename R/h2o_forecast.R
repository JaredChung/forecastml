
# ========================================== #
# Project automatic forecast ML
# Jared Chung
# 23/08/2017
# file: machine_learning_forecast
# ========================================== #

#' Forecast using h2o Machine learning techniques
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


#https://github.com/h2oai/h2o-tutorials/blob/master/h2o-open-tour-2016/chicago/grid-search-model-selection.R


###################################
# H2o
####################################


forecast_h2o <- function(train,
                         test,
                         seed = 42,
                         run_automl = FALSE) {

  # Convert into dataframe if object is TS

  if(class(train) == "ts") {
    train <- data.frame(list(date = as.Date(lubridate::date_decimal(as.numeric(time(train)))),
                            value = as.numeric(train)))
    print("Object is Time Series")
  }
  if(class(test) == "ts") {
    test <- data.frame(list(date = as.Date(lubridate::date_decimal(as.numeric(time(test)))),
                            value = as.numeric(test)))
    print("Object is Time Series")
  }


  # h2o initilization
  init <- h2o::h2o.init(strict_version_check = FALSE, nthreads = -1)

  # Convert into h2o data frame
  train_h2o <- h2o::as.h2o(train)
  test_h2o <- h2o::as.h2o(test)

  # assign x and y index
  y_index <- 'value'
  x_index <- setdiff(names(train_h2o), y_index)

  # Hyperparamter search function

  h2o_gridsearch <- function(grid_id,
                             sort_by = "rmse",
                             decreasing = FALSE) {

    gridperf <- h2o::h2o.getGrid(grid_id = grid_id,
                                     sort_by = sort_by,
                                     decreasing = decreasing)

    h2o_model_id <- gridperf@model_ids[[1]]
    h2o_model <- h2o::h2o.getModel(h2o_model_id)

    model_param <- as.data.frame(h2o_model@parameters)

    return(list(h2o_model = h2o_model,
                model_param = model_param))
  }

  #========================================
  # ELASTIC NET
  #========================================

  # Set range of alpha to be searched on (deciding the balance between ridge(L2) and lasso(l1))

  glm_params = list(alpha = c(0,0.25,0.5,0.75,1))

  glm_grid <- h2o::h2o.grid("glm",
                           x = x_index,
                           y = y_index,
                           grid_id = "glm_grid",
                           training_frame = train_h2o,
                           validation_frame = test_h2o,
                           family = "gaussian",
                           lambda_search = TRUE,
                           standardize = TRUE,
                           seed = seed,
                           hyper_params = glm_params)

  # Get the grid results, sorted by AUC

  glm_grid_details <- h2o_gridsearch("glm_grid")

  glm_h2o <- glm_grid_details$h2o_model

  glm_model_param <- glm_grid_details$model_param

  #========================================
  # RANDOM FOREST
  #========================================

  rf_params = list(ntrees = c(100,300,500),
                   max_depth = c(3, 6, 9))

  rf_grid <- h2o::h2o.grid("randomForest",
                            x = x_index,
                            y = y_index,
                            grid_id = "rf_grid",
                            training_frame = train_h2o,
                            validation_frame = test_h2o,
                            seed = seed,
                            hyper_params = rf_params)

  # Get the grid results, sorted by AUC

  rf_grid_details <- h2o_gridsearch("rf_grid")

  rf_h2o <- rf_grid_details$h2o_model

  rf_model_param <- rf_grid_details$model_param


  #========================================
  # GRADIENT BOOSTED MACHINE
  #========================================

  gbm_params <- list(learn_rate = c(0.01, 0.05, 0.1),
                      max_depth = c(3, 6, 9),
                      sample_rate = c(0.6, 0.8, 1.0),
                      col_sample_rate = c(0.2, 0.5, 1.0),
                      ntrees = c(100,300,500))

  # Train and validate a grid of GBMs
  gbm_grid <- h2o::h2o.grid("gbm",
                        x = x_index,
                        y = y_index,
                        grid_id = "gbm_grid",
                        training_frame = train_h2o,
                        validation_frame = test_h2o,
                        seed = seed,
                        hyper_params = gbm_params)


  # Get the grid results, sorted by rmse
  gbm_grid_details <- h2o_gridsearch("gbm_grid")

  gbm_h2o <- gbm_grid_details$h2o_model

  gbm_model_param <- gbm_grid_details$model_param


  #========================================
  # MULTILAYER PERCEPTRON
  #========================================

  activation_opt <- c("Rectifier", "RectifierWithDropout", "Maxout", "MaxoutWithDropout")
  l1_opt <- c(0, 0.00001, 0.0001, 0.001, 0.01, 0.1)
  l2_opt <- c(0, 0.00001, 0.0001, 0.001, 0.01, 0.1)
  hyper_params <- list(activation = activation_opt,
                       l1 = l1_opt,
                       l2 = l2_opt)

  mlp_h2o <- h2o::h2o.deeplearning(
                        model_id="mlp_model",
                        x = x_index,
                        y = y_index,
                        training_frame=train_h2o,
                        validation_frame=test_h2o,   ## validation dataset: used for scoring and early stopping
                        activation="Rectifier",  ## default
                        hidden=c(200,200),       ## default: 2 hidden layers with 200 neurons each
                        epochs=1,
                        variable_importances=T    ## not enabled by default
                        )

  #========================================
  # AUTOMATIC MACHINE LEARNING
  #========================================

  if(run_automl) {
    automl_h2o <- h2o::h2o.automl(x = x_index,
                                y = y_index,
                                training_frame = train_h2o,
                                validation_frame = test_h2o,
                                #leaderboard_frame = test_h2o,
                                max_runtime_secs = 3300,
                                stopping_metric = "AUTO")
  } else {
    automl_h2o <- NA
  }


  # Model Results

  # To store data

  results <- matrix(nrow = 1,ncol = 5)

  results[1, 1] <- h2o::h2o.rmse(glm_h2o, valid=T)
  results[1, 2] <- h2o::h2o.rmse(rf_h2o, valid=T)
  results[1, 3] <- h2o::h2o.rmse(gbm_h2o, valid=T)
  results[1, 4] <- h2o::h2o.rmse(mlp_h2o, valid=T)

  if(run_automl) {
    results[1,5] <- h2o::h2o.rmse(automl_h2o, valid=T)
  } else {
    results[1,5] <- NA
  }


  results <- as.data.frame(results)
  colnames(results) <- c("glm","rf","gbm","mlp","auto_ml")

  models <- list(glm = glm_h2o,
                 rf = rf_h2o,
                 gbm = gbm_h2o,
                 mlp = mlp_h2o,
                 auto_ml = automl_h2o)

  #predictions <- as.h2o.predict(glm_h2o , newdata = )

  model_param <- list(glm = glm_model_param,
                      gbm = gbm_model_param,
                      rf = rf_model_param)

  h2o::h2o.shutdown(prompt=FALSE)


  return(list(results = results,
              models = models,
              model_param = model_param))

}


h2o_fitting <- function() {

}


#
# Testing

# Run
#
# library(fpp2)
#
# data <- a10
#
# x_reg <- fit_feature_extracter(data, num_lag = 2, num_roll = 3)
#
# cv_horizon <- 1
# intitial_window <- 0.7
#
# trainslices <- cross_validation_data(data,
#                                      initialwindow = intitial_window,
#                                      horizon = cv_horizon)$train
# testslices <- cross_validation_data(data,
#                                     initialwindow = intitial_window,
#                                     horizon = cv_horizon)$test
#
# trainslices_xreg <- cross_validation_data(x_reg,
#                                           initialwindow = intitial_window,
#                                           horizon = cv_horizon)$train
# testslices_xreg <- cross_validation_data(x_reg,
#                                          initialwindow = intitial_window,
#                                          horizon = cv_horizon)$test
#
#
# # with external regressors
# result <- forecast_h2o(train = x_reg[trainslices_xreg[[1]],],
#                        test = x_reg[testslices_xreg[[1]],])



# # without external regressors
# result <- forecast_h2o(train = data[trainslices[[1]]],
#                        test = data[testslices[[1]]])


#h2o::h2o.shutdown(prompt=FALSE)



#TESTING h2o

# data <- a10
#
# cv_horizon <- 1
# intitial_window <- 0.7
#
# trainslices <- cross_validation_data(data,
#                                      initialwindow = intitial_window,
#                                      horizon = cv_horizon)$train
# testslices <- cross_validation_data(data,
#                                     initialwindow = intitial_window,
#                                     horizon = cv_horizon)$test
#
# trainslices_xreg <- cross_validation_data(external_regressor,
#                                           initialwindow = intitial_window,
#                                           horizon = cv_horizon)$train
# testslices_xreg <- cross_validation_data(external_regressor,
#                                          initialwindow = intitial_window,
#                                          horizon = cv_horizon)$test
#
#
# h2o.init()
#
# data <- data.frame(list(date = as.Date(time(data)),
#                         value = as.numeric(data)))
#
# data <- as.h2o(data)
#
# # GBM hyperparamters
# gbm_params1 <- list(learn_rate = c(0.01, 0.1),
#                     max_depth = c(3, 5, 9),
#                     sample_rate = c(0.8, 1.0),
#                     col_sample_rate = c(0.2, 0.5, 1.0))
#
# # Train and validate a grid of GBMs
# gbm_grid1 <- h2o.grid("gbm", x = "date", y = "value",
#                       grid_id = "gbm_grid1",
#                       training_frame = data[trainslices[[1]],],
#                       validation_frame = data[testslices[[1]],],
#                       ntrees = 100,
#                       seed = 1,
#                       hyper_params = gbm_params1)
#
#
# # Get the grid results, sorted by AUC
# gbm_gridperf1 <- h2o.getGrid(grid_id = "gbm_grid1",
#                              sort_by = "rmse",
#                              decreasing = FALSE)
#
#
# best_gbm_model_id <- gbm_gridperf1@model_ids[[1]]
# best_gbm <- h2o.getModel(best_gbm_model_id)
#
# model_param <- as.data.frame(best_gbm@parameters)
#
#
# alpha_opts = list(list(0), list(.25), list(.5), list(.75), list(1))
# hyper_parameters = list(alpha = alpha_opts)
#
# glm_h2o <- h2o::h2o.glm(x = x_index,
#                         y = y_index,
#                         training_frame = train_h2o,
#                         validation_frame = test_h2o,
#                         seed = seed,
#                         family = "gaussian",
#                         lambda_search = TRUE,
#                         standardize = TRUE,
#                         #hyper_params = hyper_parameters,
#                         nfolds = 5)
#
# #
# rf_h2o <- h2o::h2o.randomForest(x = x_index,
#                                 y = y_index,
#                                 training_frame = train_h2o,
#                                 validation_frame = test_h2o,
#                                 seed = seed,
#
# h2o.shutdown()





###################################
# Mxnet
####################################




###################################
# Lightgbm
####################################





###################################
# Catboost
####################################




###################################
# Fitting of models with cross validation
####################################










