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


#https://github.com/bhimmetoglu/kaggle_101/blob/master/HousePrices/modelsExplore/exploreE1071.R

###################################
# Caret
####################################


caret_forecast <- function (train,
                            test,
                            seed=42){


      set.seed(seed)

      train_control <- caret::trainControl(method = "cv",
                                           number = 5)

      # Elastic net

      glmnet_grid <- expand.grid(lambda = 10 ^ seq(2, -2, length = 100),
                                 alpha = seq(0, 1, length = 10))

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

      glm_predict <- predict(glmnet_model_final, newdata = test)

      result <- error_metric(test = test$value, prediction = glm_predict)

      # Random Forest




      # Gradient Boosted Machine


      grid_gbm <- expand.grid(interaction.depth=c(1,3,7,10), # Depth of variable interactions
                              n.trees=c(100,200),	        # Num trees to fit
                              shrinkage=seq(0.1,1, by=0.2),
                              n.minobsinnode = 20)
      #
       # set the seed

      # Set up to do parallel processing
      registerDoParallel(parallel::detectCores())		# Registrer a parallel backend for train
      getDoParWorkers()

      gbm.tune <- train(x=trainX,y=trainData$Class,
                        method = "gbm",
                        #metric = "ROC",
                        trControl = ctrl,
                        tuneGrid = grid_gbm,
                        verbose = FALSE)


      result <- data.frame(model = name,
                           timeslice = timeslice,
                           error = error_metric(test,as.numeric(predictions$mean)))


    return(list())

}






library(fpp2)

data <- a10

trainslices <- cross_validation_data(data)$train
testslices <- cross_validation_data(data)$test


fit <- caret_forecast(data[trainslices[[1]]],
                      data[testslices[[1]]])






