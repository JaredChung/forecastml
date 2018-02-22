

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



library(data.table)
library(lightgbm)

data(agaricus.train, package = "lightgbm")
train <- agaricus.train
dtrain <- lgb.Dataset(train$data, label = train$label, free_raw_data = FALSE)

data(agaricus.test, package = "lightgbm")
test <- agaricus.test
dtest <- lgb.Dataset.create.valid(dtrain, test$data, label = test$label)

valids <- list(test = dtest)

grid_search <- expand.grid(max_depth = c(10,20,40,80),
                           min_data_in_leaf = c(1,2,4),
                           min_sum_hessian_in_leaf = c(0.05,0.1,0.2),
                           feature_fraction = c(0.8,0.9,0.95),
                           bagging_fraction = c(0.4,0.6),
                           bagging_freq = c(2,4),
                           lambda_l1 = c(0.2,0.4),
                           lambda_l2 = c(0.2,0.4),
                           min_gain_to_split = c(0.2,0.4))

perf <- numeric(nrow(grid_search))

for (i in 1:nrow(grid_search)) {
  model <- lgb.train(list(objective = "regression",
                          metric = "l2",
                          max_depth = grid_search[i, "max_depth"],
                          min_data_in_leaf = grid_search[i,"min_data_in_leaf"],
                          min_sum_hessian_in_leaf = grid_search[i, "min_sum_hessian_in_leaf"],
                          feature_fraction =  grid_search[i, "feature_fraction"],
                          bagging_fraction =  grid_search[i, "bagging_fraction"],
                          bagging_freq =  grid_search[i, "bagging_freq"],
                          lambda_l1 =  grid_search[i, "lambda_l1"],
                          lambda_l2 =  grid_search[i, "lambda_l2"],
                          min_gain_to_split =  grid_search[i, "min_gain_to_split"]),
                     dtrain,
                     2,
                     valids,
                     #min_data = 1,
                     num_leaves = 100,
                     learning_rate = 0.1,
                     early_stopping_rounds = 20)
  perf[i] <- min(rbindlist(model$record_evals$test$l2))
  gc(verbose = FALSE)
}
# grid_search
cat("Model ", which.min(perf), " is lowest loss: ", min(perf), sep = "","\n")
print(grid_search[which.min(perf), ])
