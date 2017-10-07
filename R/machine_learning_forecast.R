
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


# Incorporate below Modelling Code

library(h2o)

h2o.init(strict_version_check = FALSE)
# show general information on the airquality dataset
colnames(airquality)
dim(airquality)
print(paste(‘number of months:’,length(unique(airquality$Month)), sep=“”))
# add a year column, so you can create a month, day, year date stamp
airquality$Year <- rep(2017,nrow(airquality))
airquality$Date <- as.Date(with(airquality, paste(Year, Month, Day,sep=“-“)), “%Y-%m-%d”)
# sort the dataset
airquality <- airquality[order(as.Date(airquality$Date, format=“%m/%d/%Y”)),]
# convert the dataset to unix time before converting to an H2OFrame
airquality$Date <- as.numeric(as.POSIXct(airquality$Date, origin=“1970-01-01”, tz = “GMT”))
# convert to an h2o dataframe
air_h2o <- as.h2o(airquality)
# specify the features and the target column
target <- ‘Ozone’
features <- c(“Solar.R”, “Wind”, “Temp”,  “Month”, “Day”, “Date”)
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




















