
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

      data <- data %>% mutate(month = month(data),
                              day = )

      return(data)
}


# Test
library(fpp2)

data <- a10

data_extract <- feature_extracter(data)

date =

