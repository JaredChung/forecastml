
# ========================================== #
# Project automatic forecast ML
# Jared Chung
# 23/08/2017
# file: Standard forecasting techniques
# ========================================== #

#' Forecast using standard forecasting
#'
#' @param data Time series object as an input
#' @param
#'
#' @example


library(fpp2) # data to test time series on
library(forecast)
library(ggplot2)
library(caret)
library(tidyverse)
library(zoo)
library(lubridate)


# load test data of pharmaceutical products
data <- a10


# Standard 80/20 train test split
preprocess_data <- function(data) {

    if("ts" %in% class(data)) {
      data <- data.frame(date = as.Date(yearmon(time(data))), value=  as.matrix(data))
      train = data[1:floor(nrow(data)*0.8),]
      test = data[(floor(nrow(data)*0.8)+1):nrow(data),]
    } else {
      train = data[1:floor(nrow(data)*0.8),]
      test = data[(floor(nrow(data)*0.8)+1):nrow(data),]
    }
    return(list(train=train,test=test))

}


cross_validation_data <- function(data) {


}


standard_forecast <- function(data,start = ,cv_horizon = 12) {

          #

          if ("ts" %in% class(data)) {
              return(timeslice <- createTimeSlices(1:length(data),initialWindow = length(data) * 0.7, horizon = 12, fixedWindow = TRUE))
          }else {
              return(timeslice <- createTimeSlices(1:nrow(data),initialWindow = nrow(data) * 0.7, horizon = 12, fixedWindow = TRUE))
          }


          trainslices <- timeslice$train
          testslices <- timeslice$test

          predictions <- data.frame()
          results <- data.frame()


          # testing models on sample
          model <- forecast::ets(data[trainslices[[1]]],lambda=BoxCox.lambda(data))

          model2 <- forecast::auto.arima(data[trainslices[[1]]],lambda=BoxCox.lambda(data))

          predictions <- forecast::forecast(model,h=length(data[testslices[[1]]]))

          predictions2 <- forecast::forecast(model2,h=length(data[testslices[[1]]]))

          result <- accuracy(predictions,data[testslices[[1]]])

          result2 <- accuracy(predictions2,data[testslices[[1]]])



          # Cross validation time series
          for(i in 1:length(timeslice)) {

            model <- forecast::ets(data[trainslices[[i]]])

            predictions <- forecast::forecast(model,h=length(data[testslices[[i]]]))


          }

          # Automatic Expoinential smoothing
          model <- forecast::ets(train)

          # Automatic Arima
          model2 <- forecast::auto.arima(train)

          # Automatic Exponential smoothing + ARMA + Box Cox transformations
          model3 <- forecast::tbats(train)

          # Basic single hidden layer neural network
          model4 <- forecast::nnetar(train)

          # Simple Exponential Smoothing
          model5 <- forecast::thetaf(train)


          model6 <-

          return()
}



for(i in 1:length(trainSlices)){
  plsFitTime <- train(unemploy ~ pce + pop + psavert,
                      data = economics[trainSlices[[i]],],
                      method = "pls",
                      preProc = c("center", "scale"))
  pred <- predict(plsFitTime,economics[testSlices[[i]],])


  true <- economics$unemploy[testSlices[[i]]]
  plot(true, col = "red", ylab = "true (red) , pred (blue)",
       main = i, ylim = range(c(pred,true)))
  points(pred, col = "blue")
}



plot(a10, ylab="$ million", xlab="Year", main="Antidiabetic drug sales")
plot(log(a10), ylab="", xlab="Year", main="Log Antidiabetic drug sales")

k <- 60 # minimum data length for fitting a model
n <- length(a10)
mae1 <- mae2 <- mae3 <- matrix(NA,n-k,12)
st <- tsp(a10)[1]+(k-2)/12

for(i in 1:(n-k))
{
  xshort <- window(a10, end=st + i/12)
  xnext <- window(a10, start=st + (i+1)/12, end=st + (i+12)/12)
  fit1 <- tslm(xshort ~ trend + season, lambda=0)
  fcast1 <- forecast(fit1, h=12)
  fit2 <- Arima(xshort, order=c(3,0,1), seasonal=list(order=c(0,1,1), period=12),
                include.drift=TRUE, lambda=0, method="ML")
  fcast2 <- forecast(fit2, h=12)
  fit3 <- ets(xshort,model="MMM",damped=TRUE)
  fcast3 <- forecast(fit3, h=12)
  mae1[i,1:length(xnext)] <- abs(fcast1[['mean']]-xnext)
  mae2[i,1:length(xnext)] <- abs(fcast2[['mean']]-xnext)
  mae3[i,1:length(xnext)] <- abs(fcast3[['mean']]-xnext)
}

plot(1:12, colMeans(mae1,na.rm=TRUE), type="l", col=2, xlab="horizon", ylab="MAE",
     ylim=c(0.65,1.05))
lines(1:12, colMeans(mae2,na.rm=TRUE), type="l",col=3)
lines(1:12, colMeans(mae3,na.rm=TRUE), type="l",col=4)
legend("topleft",legend=c("LM","ARIMA","ETS"),col=2:4,lty=1)



