#' Categorizes the time stamps in a data frame daily
#'
#' @param data A data frame
#'
#' @return Returns the data frame with an additional column referring to the
#' day time of each time stamp
#' @export
#'
daily_decomposition <- function(data) {

  #Define day/night beginning
  DAY <- 25200
  NIGHT <- 82800

  #Extract ONLY time from time stamps
  d <- substr(data$X, 12, 19)
  data$time <- d
  data$daytime <- NaN

  #Calculate seconds passed since 00:00:00
  data$time = as.numeric(substring(data$time, 7, 8)) + 60*(as.numeric(substring(data$time, 4, 5))
                                                      + 60*as.numeric(substring(data$time, 1, 2)))

  #Determine day or night
  data$daytime[data$time >= DAY & data$time < NIGHT] <- "07-23"
  data$daytime[data$time >= NIGHT | data$time < DAY] <- "23-07"

  #Delete temporary  time column
  data$time <- NULL

  return(data)
}

