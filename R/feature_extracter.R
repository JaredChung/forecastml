
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

feature_extracter <- function(data, date_col = FALSE, num_lag = 2) {

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

      return(new_data)
}


data <- a10

data_extract <- feature_extracter(data, num_lag =2)









