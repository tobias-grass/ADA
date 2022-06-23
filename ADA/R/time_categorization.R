time_categorization <- function(data_frame, formatted = TRUE, tz = "UTC",
                                format = "%Y-%m-%d %H:%M:%S") {

  #---------------------------------------------------------
  # sanity checks
  #---------------------------------------------------------

  stopifnot(is.data.frame(data_frame))
  stopifnot(is.logical(formatted))

  #---------------------------------------------------------
  # function body
  #---------------------------------------------------------

  if (!formatted) {
    data_frame = formatting(data_frame = data_frame, tz = tz, )
  }

}
