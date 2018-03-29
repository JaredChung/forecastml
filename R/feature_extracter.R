
# ========================================== #
# Project automatic forecast ML
# Jared Chung
# 23/08/2017
# file: feature_extracter
# ========================================== #

#' Extract time series based features
#'
#' @param data Time series object as an input
#' @param num_lag
#' @param num_roll
#' @param fourier_k
#'
#' @example
#'
#'
#'
#' @export


#fit extracter

fit_feature_extracter <- function(data, date_col = FALSE, num_lag = 2, num_roll = 3, fourier_K = 3) {


  if(class(data) == "ts") {
    new_data <- data.frame(list(date = as.Date(lubridate::date_decimal(as.numeric(time(data)))),
                            value = as.numeric(data)))

  }

  # Build date features (e.g year)
  new_data <- new_data %>% dplyr::mutate(month = lubridate::month(date),
                          day = lubridate::day(date),
                          year = lubridate::year(date),
                          #weeks = weeks(date),
                          mday = lubridate::mday(date),
                          wday = lubridate::wday(date), # remove 1 level for "dummy variable trap?
                          yday = lubridate::yday(date))

  # Create lag Features

  for (i in seq(num_lag)) {
    name = paste("lag", i, sep="")
    new_data[name] <- lag(new_data$value,i)
  }

  # Create Rolling Features


  new_data['rollmean'] <- zoo::rollmean(x = new_data$value , k = num_roll,fill= NA,na.pad=FALSE,align='right')
  new_data['rollmax'] <- zoo::rollmax(x = new_data$value , k = num_roll,fill= NA,na.pad=FALSE,align='right')
  new_data['rollsum'] <- zoo::rollsum(x = new_data$value , k = num_roll,fill= NA,na.pad=FALSE,align='right')
  new_data['rollmedian'] <- zoo::rollmedian(x = new_data$value , k = num_roll,fill= NA,na.pad=FALSE,align='right')
  new_data['rollsd'] <- zoo::rollapply(data = new_data$value, width = num_roll, FUN=sd,fill= NA,na.pad=FALSE, align='right')


  # Fourier Features

  fourier_terms <- forecast::fourier(data, K = frequency(data)/2)

  new_data <- cbind(new_data, as.data.frame(fourier_terms))

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

  #prcomp_features

  return(new_data)

}

#process feature extractor

forecast_feature_extractor <- function(h = 12) {



}


# Test
# library(fpp2)
# data <- a10
#
# data_extract <- fit_feature_extracter(data, num_lag =2, num_roll = 3, fourier_K = 6)









