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



# Clean and prepare data
#source("stacking/cleanData.R")
#select <- dplyr::select
source("stacking/cleanDataDetailed.R")

# XGBoost training
# Grid for model training

forecast_xgboost <- function(train,
                             test,
                             seed = 42) {


  xgb_grid <- expand.grid(eta = 2 ^ seq(-7,-5),
                          colsample_bytree = c(0.2, 0.4, 0.6, 0.8),
                          max_depth = c(2, 4, 6, 8),
                          min_child_weight = c(0, 1, 2, 4),
                          gamma = c(0,0.001,0.01, 0.1))

  # xgb style matrices
  dtrain <- xgb.DMatrix(trainSparse, label = outcomes)
  dtest <- xgb.DMatrix(testSparse)

  # Loop over parameters
  watchlist <- list(train=dtrain)
  cv.results <- data.frame(xgb_grid); cv.results$nrounds = 0; cv.results$rmse = 0
  for (ind in 1:dim(xgb_grid)[1]){
    # Model parameters
    eta <- xgb_grid[ind,1]; colsample_bytree <- xgb_grid[ind,2]; max_depth <- xgb_grid[ind,3]
    min_child_weight = xgb_grid[ind,4]; gamma <- xgb_grid[ind,5]
    #
    param <- list(booster="gbtree",
                  eval_metric="rmse",
                  eta=eta,
                  colsample_bytree = colsample_bytree,
                  max_depth = max_depth,
                  min_child_weight = min_child_weight,
                  gamma = gamma,
                  lambda = 1.0,
                  subsample = 0.8)
    # 5-fold CV
    set.seed(11111)
    fit_cv <- xgb.cv(params=param,
                     data=dtrain,
                     nrounds=1000,
                     watchlist=watchlist,
                     nfold=5,
                     early_stopping_rounds = 3)

    cv.results[ind,6] <- fit_cv$best_iteration
    cv.results[ind,7] <- fit_cv$evaluation_log[fit_cv$best_iteration][[4]]
    cat("Trained ", ind, " of ", dim(xgb_grid)[1], "\n")
    # Save cv_results
    save(cv.results, file = "xgb_cv10.RData")

    # which parameters yield minimum rmse?
    ind.min <- which.min(cv.results$rmse)
  }

}


# Reference (lambda = 1 default, no reomval of outliers beyond GrLivArea)
# eta          colsample_bytree   max_depth    min_child_weight gamma nrounds     rmse
# 0.015625              0.4         4                2           0.01     836    0.113743
##
#  eta           colsample_bytree  max_depth  min_child_weight gamma nrounds     rmse
# 0.015625              0.2           4                2         0     914       0.108657
# Final Model fit
param <- list(booster="gbtree",
              eval_metric="rmse",
              eta=0.015625,
              colsample_bytree = 0.4,
              max_depth = 4,
              min_child_weight = 2,
              gamma = 0.01,
              lambda = 1.0,
              subsample = 0.8)

mod.xgb <- xgboost(data=dtrain, params = param, nrounds=836)

# Predict on test set
predTest <- predict(mod.xgb, newdata = dtest)
Ids <- test$Id # Id numbers

# Data for submission
submission <- data.frame(Id = Ids, SalePrice = exp(predTest))
write.csv(submission,"submission-xgb.csv",row.names = FALSE)

# Feature importances
importance <- xgb.importance(feature_names = trainSparse@Dimnames[[2]], model = mod.xgb)
head(importance,10)

### ---- Explore wrong predictions ---- ####

# Look at training errors
predTrain <- predict(mod.xgb, newdata = dtrain)
train <- train %>% mutate(y_actual = outcomes) %>% mutate(y_pred = predTrain) %>% mutate(diff = abs(y_actual-y_pred))
# Worst ones
badPreds <- train %>% filter(diff > 0.3) %>% arrange(desc(diff))

# Plot
gg <- ggplot(train, aes(y_actual, y_pred)) + geom_point(aes(x = y_actual, y = y_pred, color = diff)) +
  geom_abline(slope = 1, intercept = 0) + geom_point(data=badPreds, colour="red") +
  scale_colour_gradient(limits=c(0, 0.35)) + ggtitle("XGBoost Predictions on Training Set")
gg

# # XGBoost training
# library(xgboost)
# library(caret)
#
# # Grid for model training
# xgb_grid <- expand.grid(eta = 2^seq(-7,-5), lambda = c(0,0.01,0.1,1), alpha = c(0,0,01,0.1,1),
#                         lambda_bias = c(0,0.01,0.1,1),min_child_weight = c(1,2,4))
#
# # xgb style matrices
# dtrain <- xgb.DMatrix(trainSparse, label = outcomes)
# dtest <- xgb.DMatrix(testSparse)
#
# # Loop over parameters
# watchlist <- list(train=dtrain)
# cv.results <- data.frame(xgb_grid); cv.results$nrounds = 0; cv.results$rmse = 0
# for (ind in 1:dim(xgb_grid)[1]){
#   # Model parameters
#   eta <- xgb_grid[ind,1]; lambda <- xgb_grid[ind,2]; alpha <- xgb_grid[ind,3]
#   lambda_bias <- xgb_grid[ind,4]
#   #
#   param <- list(booster="gblinear",
#                 eval_metric="rmse",
#                 eta=eta,
#                 lambda = lambda,
#                 alpha = alpha,
#                 lambda_bias = lambda_bias,
#                 subsample = 0.8)
#   # 5-fold CV
#   set.seed(11111)
#   fit_cv <- xgb.cv(params=param,
#                    data=dtrain,
#                    nrounds=1000,
#                    watchlist=watchlist,
#                    nfold=5,
#                    early_stopping_rounds = 3)
#
#   cv.results[ind,5] <- fit_cv$best_iteration
#   cv.results[ind,6] <- fit_cv$evaluation_log[fit_cv$best_iteration][[4]]
#   cat("Trained ", ind, " of ", dim(xgb_grid)[1], "\n")
# }
# # Save cv_results
# save(cv.results, file = "xgblinear_cv.RData")
#
# # which parameters yield minimum rmse?
# ind.min <- which.min(cv.results$rmse)
#
# # Reference
# # eta       lambda alpha   lambda_bias  nrounds      rmse
# # 0.03125      1     0           0        1000     0.2404744
#
# # Final Model fit
# param <- list(booster="gblinear",
#               eval_metric="rmse",
#               eta=0.03125,
#               lambda = 1.0,
#               alpha = 0.0,
#               lambda_bias = 0.0,
#               min_child_weight = 1,
#               subsample = 0.8)
#
# mod.xgb <- xgboost(data=dtrain, params = param, nrounds=1000)
#
# # Predict on test set
# predTest <- predict(mod.xgb, newdata = dtest)
# Ids <- test$Id # Id numbers
#
# # Data for submission
# submission <- data.frame(Id = Ids, SalePrice = exp(predTest))
# write.csv(submission,"submission.csv",row.names = FALSE)
#
# # Feature importances
# importance <- xgb.importance(feature_names = trainSparse@Dimnames[[2]], model = mod.xgb)
# head(importance,10)







####################################
# Alternate strategy for running grid search for xgboost
#####################################

# searchGridSubCol <- expand.grid(subsample = c(0.5, 0.75, 1),
#                                 colsample_bytree = c(0.6, 0.8, 1))
# ntrees <- 100
#
# #Build a xgb.DMatrix object
# DMMatrixTrain <- xgb.DMatrix(data = yourMatrix, label = yourTarget)
#
# rmseErrorsHyperparameters <- apply(searchGridSubCol, 1, function(parameterList){
#
#   #Extract Parameters to test
#   currentSubsampleRate <- parameterList[["subsample"]]
#   currentColsampleRate <- parameterList[["colsample_bytree"]]
#
#   xgboostModelCV <- xgb.cv(data =  DMMatrixTrain, nrounds = ntrees, nfold = 5, showsd = TRUE,
#                            metrics = "rmse", verbose = TRUE, "eval_metric" = "rmse",
#                            "objective" = "reg:linear", "max.depth" = 15, "eta" = 2/ntrees,
#                            "subsample" = currentSubsampleRate, "colsample_bytree" = currentColsampleRate)
#
#   xvalidationScores <- as.data.frame(xgboostModelCV)
#   #Save rmse of the last iteration
#   rmse <- tail(xvalidationScores$test.rmse.mean, 1)
#
#   return(c(rmse, currentSubsampleRate, currentColsampleRate))
#
# })



