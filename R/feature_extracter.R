
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

feature_extracter <- function(data, date_col = FALSE, num_lag = FALSE) {

      require(lubridate)
      require(dplyr)
      require(zoo)

      if(class(data) == "ts") {
          data <- as.data.frame(list(date =as.Date(yearmon(time(data)))))
          data$value <- as.data.frame(list(value=as.data.frame(data)))

      } else

      data <- data %>% mutate(month = month(date),
                              day = day(date),
                              year = year(date))
                   %>% mutate( lag1 = )


      return(data)
}


data <- a10

data_extract <- feature_extracter(data)

data
