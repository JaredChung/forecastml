
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



feature_extracter <- function(data) {

      require(lubridate)
      require(dplyr)
      require(zoo)

      if(class(data) == "ts") {
            data <- as.Date(yearmon(time(data)))
      }

      #data <- data %>%

      return(data)
}


# Test
library(fpp)

data <- a10

feature

date =

