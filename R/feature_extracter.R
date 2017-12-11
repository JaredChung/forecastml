
# ========================================== #
# Project automatic forecast ML
# Jared Chung
# 23/08/2017
# file: feature_extracter
# ========================================== #

#' Forecast using standard forecasting
#'
#' @param data Time series object as an input
#' @param
#'
#' @example


#tempory import
library(zoo)
library(parallel)

#fit extracter

fit_feature_extracter <- function(data, date_col = FALSE, num_lag = 2, num_roll = 3, fourier_K = 3) {

      require(lubridate)
      require(dplyr)
      require(zoo)

      if(class(data) == "ts") {
          new_data <- as.data.frame(list(date =as.Date(yearmon(time(data)))))
          new_data <- cbind(new_data,as.data.frame(list(value=data)))
          new_data$value <- as.numeric(new_data$value)

      }

      # Build date features (e.g year)
      new_data <- new_data %>% mutate(month = month(date),
                              day = day(date),
                              year = year(date),
                              weeks = weeks(date),
                              mday = mday(date),
                              wday = wday(date),
                              yday = yday(date))

      # Create lag Features

      for (i in seq(num_lag)) {
            name = paste("lag",i,sep="")
            new_data[name] <- lag(new_data$value,i)
      }

      # Create Rolling Features


      new_data['rollmean'] <- zoo::rollmean(x = new_data$value , k = num_roll,fill= NA,na.pad=FALSE,align='right')
      new_data['rollmax'] <- zoo::rollmax(x = new_data$value , k = num_roll,fill= NA,na.pad=FALSE,align='right')
      new_data['rollsum'] <- zoo::rollsum(x = new_data$value , k = num_roll,fill= NA,na.pad=FALSE,align='right')
      new_data['rollmedian'] <- zoo::rollmedian(x = new_data$value , k = num_roll,fill= NA,na.pad=FALSE,align='right')
      new_data['rollsd'] <- zoo::rollapply(data = new_data$value, width = num_roll, FUN=sd,fill= NA,na.pad=FALSE, align='right')

      # Create Holiday Days (UNDER CONSTRUCITON)

      # Symmetry_looking (UNDER CONSTRUCITON)

      # mean_difference = abs(mean(new_data$value)-median(new_data$value))
      # max_min_difference = max(new_data$value) - min(new_data$value)
      #
      # for (n in seq(0.0,0.95,0.05)) {
      #
      #       if (mean_difference < n * max_min_difference)
      #
      # }
      #
      # #
      #
      # return(new_data)

      # Fourier Features

      new_data['fourier'] <- fourier(new_data$value,K = fourier_K)

}

#process feature extractor

process_feature_extractor <- function(x) {

}


# Test
#library(fpp2)
#data <- a10

#data_extract <- fit_feature_extracter(data, num_lag =2, num_roll = 3,fourier_K =2)









