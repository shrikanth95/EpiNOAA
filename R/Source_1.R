load_data <- function(name, fileName){
  return(1)

}

generateTimeSeries <- function(name, fileName){
  return(1)

}



interpolateData <- function(NOAAData) {
  return(1)
}

#' Creates a synthetic dataset for tests with observations defined by
#'  `number_samples` and columns `date`,`FIPS Code`,`County`, `t_{avg}`,
#'  `t_{min}` and `t_{max}`
#' @param number_samples The number of samples in the test dataset.
#' @return A synthetic NOAA dataframe.
createSyntheticData <- function(number_samples) {

}

#' Calculate Basis Summary Statistics
#' This function generates summary statistics for the cleaned NOAA dataset for
#' a predefined spatial and temporal resolution.
#' @param noaa_clean NOAA time series data that's cleaned and interpolated.
#' @param time_resolution Desired temporal resolution. Must numeric.
#' Defaults to the number of hours.
#' @param time_resolution_unit Units for the time_resolution. Must be c("auto",
#'  "secs", "mins", "hours", "days", "weeks")
#' @return Basic summary statistics.
#'  @examples
#' generateSummaryStats(noaa_clean, 24)
#' generateSummaryStats(noaa_clean, 14, 'days')
#' @export

generateSummaryStats <- function(noaa_clean,
                                 time_resolution,
                                 time_resolution_unit = 'hours'){

  return(1)
}
# Notes to self:
# - never use `library()`. Use `dplyr::data %>% ` instead
# - Regarding documentation:
#   - see comments above the function name
