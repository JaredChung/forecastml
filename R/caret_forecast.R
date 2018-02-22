# ========================================== #
# Project automatic forecast ML
# Jared Chung
# 23/08/2017
# file: machine_learning_forecast
# ========================================== #

#' Forecast using caret based Machine learning techniques
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




###################################
# Caret
####################################


caret_forecast <- function (train,
                            test){


      train_control <- caret::trainControl(method = "cv",
                                           number = 10)

      # Elastic net
      lambda_grid <- 10 ^ seq(2, -2, length = 100)
      alpha_grid <- seq(0, 1, length = 10)

      glmnet_grid <- expand.grid(lambda = lambda_grid,
                                 alpha = alpha_grid)

      glmnet_model <- train(value ~ .,
                            data = train,
                            method = "glmnet",
                            tuneGrid = glmnet_grid,
                            trControl = train_control,
                            standardize = TRUE,
                            maxit = 1000000)

      #glmnet_model <- glmnet_model$bestTune

      glmnet_model_final <- glmnet_model$finalModel

      glmnet_coef <- coef(gmlnet_model_final, s = glmnet_model$bestTune$lambda)


      # Random Forest




      # Gradient Boosted Machine


      grid <- expand.grid(interaction.depth=c(1,2), # Depth of variable interactions
                          n.trees=c(10,20),	        # Num trees to fit
                          shrinkage=c(0.01,0.1),		# Try 2 values for learning rate
                          n.minobsinnode = 20)
      #
      set.seed(1951)  # set the seed

      # Set up to do parallel processing
      registerDoParallel(4)		# Registrer a parallel backend for train
      getDoParWorkers()

      gbm.tune <- train(x=trainX,y=trainData$Class,
                        method = "gbm",
                        metric = "ROC",
                        trControl = ctrl,
                        tuneGrid=grid,
                        verbose=FALSE)







}






library(fpp2)

data <- a10

trainslices <- cross_validation_data(data)$train
testslices <- cross_validation_data(data)$test


fit <- auto.arima(data[trainslices[[1]]])

predict <- forecast(fit,h = 1)

error_metric(data[testslices[[1]]],as.numeric(predict$mean))

forecast::accuracy(predict,data[testslices[[1]]])





