
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


# Test
library(fpp2)
library(zoo)

feature_extracter <- function(data, date_col = FALSE, num_lag = 2, num_roll = 3) {

      require(lubridate)
      require(dplyr)
      require(zoo)

      if(class(data) == "ts") {
          new_data <- as.data.frame(list(date =as.Date(yearmon(time(data)))))
          new_data <- cbind(new_data,as.data.frame(list(value=data)))
          new_data$value <- as.numeric(new_data$value)

      }

      # Build date features
      new_data <- new_data %>% mutate(month = month(date),
                              day = day(date),
                              year = year(date))

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

      return(new_data)
}


data <- a10

data_extract <- feature_extracter(data, num_lag =2, num_roll = 3)









