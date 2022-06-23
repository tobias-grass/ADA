statistics <- function(data_frame, season = "ALL", tod = "ALL") {

  #---------------------------------------------------------
  # sanity checks
  #---------------------------------------------------------

  stopifnot(is.data.frame(data_frame))
  stopifnot(is.character(season), length(season) == 1L)
  stopifnot(is.character(tod), length(tod) == 1L)

  #---------------------------------------------------------
  # function body
  #---------------------------------------------------------

  x = data_frame[,1]
  time = data_frame[,2]

  na_sum = sum(is.na(x))
  x = na.omit(x)

  val = data.frame("Pollutant ID" = toupper(names(data_frame)[1]),
                   "Period ID" = season,
                   "Time ID" = tod,
                   "Mean" = mean(x),
                   "Std" = sd(x),
                   "P2.5" = quantile(x, 0.025)[[1]],
                   "P25" = quantile(x, 0.25)[[1]],
                   "P50" = quantile(x, 0.5)[[1]],
                   "P75" = quantile(x, 0.75)[[1]],
                   "P90" = quantile(x, 0.975)[[1]],
                   "Start" = min(time),
                   "End" = max(time),
                   "Typ.SI" = (as.numeric(max(time)) - as.numeric(min(time)))/length(x),
                   "Samples" = length(x),
                   "NAs" = na_sum)

  return(val)
}
