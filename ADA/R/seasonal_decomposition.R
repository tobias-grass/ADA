#' Categorizes the time stamps in a data frame seasonally
#'
#' @param data A data frame
#'
#' @return Returns the data frame with an additional column referring to the
#' season of each time stamp
#' @export
#'
#' @examples
#' y = as.Date("2019-12-17") + 1:5
#' y = data
#' y = seasonal_decomposition(y)

seasonal_decomposition <- function(data) {

  #Define season beginning
  WS <- as.Date("2016-12-21", format = "%Y-%m-%d") # Winter
  SE <- as.Date("2016-3-20",  format = "%Y-%m-%d") # Spring
  SS <- as.Date("2016-6-21",  format = "%Y-%m-%d") # Summer
  FE <- as.Date("2016-9-23",  format = "%Y-%m-%d") # Fall

  #Cut time from time stamps and shift to 2016 (leap-year),
  #introduce new columns to data_frame
  d <- as.Date(strftime(data[,1], format="2016-%m-%d"))
  data$shiftedtime <- d
  data$seasons <- NaN

  #Categorization according to the shifted time
  data$seasons[data$shiftedtime >= WS | data$shiftedtime < SE] <- "12-02"
  data$seasons[data$shiftedtime >= SE & data$shiftedtime < SS] <- "03-05"
  data$seasons[data$shiftedtime >= SS & data$shiftedtime < FE] <- "06-08"
  data$seasons[data$shiftedtime >= FE & data$shiftedtime < WS] <- "09-11"

  #Delete temporary shifted time column
  data$shiftedtime <- NULL

  return(data)

}
