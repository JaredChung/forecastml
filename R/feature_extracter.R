
# ========================================== #
# Project automatic forecast ML
# Jared Chung
# 23/08/2017
# file: Time_Series_preprocessing
# ========================================== #

#' Forecast using standard forecasting
#'
#' @param data Time series object as an input
#' @param
#'
#' @example



feature_extracter <- function(data) {

      require(lubridate)
      require(dplyr)
      require(zoo)

      if(class(data) == "ts") {
            date <- as.Date(yearmon(time(data)))
      }

      date <- date %>%


}


# Test
library(fpp)

data <- a10
date =

