# ========================================== #
# Project automatic forecast ML
# Jared Chung
# 23/08/2017
# file: machine_learning_forecast
# ========================================== #

#' Forecast using xgboost based Machine learning techniques
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
#'
#'


searchGridSubCol <- expand.grid(subsample = c(0.5, 0.75, 1),
                                colsample_bytree = c(0.6, 0.8, 1))
ntrees <- 100

#Build a xgb.DMatrix object
DMMatrixTrain <- xgb.DMatrix(data = yourMatrix, label = yourTarget)

rmseErrorsHyperparameters <- apply(searchGridSubCol, 1, function(parameterList){

  #Extract Parameters to test
  currentSubsampleRate <- parameterList[["subsample"]]
  currentColsampleRate <- parameterList[["colsample_bytree"]]

  xgboostModelCV <- xgb.cv(data =  DMMatrixTrain, nrounds = ntrees, nfold = 5, showsd = TRUE,
                           metrics = "rmse", verbose = TRUE, "eval_metric" = "rmse",
                           "objective" = "reg:linear", "max.depth" = 15, "eta" = 2/ntrees,
                           "subsample" = currentSubsampleRate, "colsample_bytree" = currentColsampleRate)

  xvalidationScores <- as.data.frame(xgboostModelCV)
  #Save rmse of the last iteration
  rmse <- tail(xvalidationScores$test.rmse.mean, 1)

  return(c(rmse, currentSubsampleRate, currentColsampleRate))

})


