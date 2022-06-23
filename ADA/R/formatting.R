#' Formats a data frame in specific format
#'
#' @description generates a new data frame according to the formula provided and
#' adds columns referring to the specific season and time of day (tod) for each
#' time stamp. Pollutants will be on the left side, the date terms on the right
#' including season and tod. The explanatory variables will be in the middle.
#'
#' @param data_frame a data frame
#' @param formula a formula describing the specific layout of the data frame
#' @param tz a time zone like "UTC", "MET", ...
#' @param format the format in which the time stamps are given in the data frame
#'
#' @return returns a data frame which has a special format
#'
#' @seealso [base::data.frame()], [stats::formula()]
#' @export
#'
formatting <- function(data_frame, formula, tz = "UTC",
                       format = "%Y-%m-%d %H:%M:%S") {

  #---------------------------------------------------------
  # sanity checks
  #---------------------------------------------------------

  stopifnot(is.data.frame(data_frame))
  stopifnot(inherits(formula, "formula"))
  stopifnot(is.character(tz), length(tz) == 1L)
  stopifnot(is.character(format), length(format) == 1L)

  #---------------------------------------------------------
  # function body
  #---------------------------------------------------------

  #create new data frame in standardized form; lhs(poll) ~ rhs1(param) + rhs2(date)
  formula <- Formula(formula)
  mf <- model.frame(formula = formula, data = data_frame, na.action = na.pass)

  #rename column and standardize time to object of class POSIXt if necessary
  if (!colnames(mf)[ncol(mf)] == "date") {
    colnames(mf)[ncol(mf)] <- "date"
  }

  if (!inherits(mf$date, "POSIXt")) {
    mf$date <- as.POSIXct(mf$date, tz = tz, format = format)
  }

  #categorize time to specific season and time of day (tod)
  mf$season <- as.integer(format(mf$date, "%m%d"))
  mf$season <- cut(x = mf$season, breaks = c(-Inf, 329, 620, 922, 1220, Inf),
                   labels = c("12-02", "03-05", "06-08", "09-11", "12-02"))

  mf$tod <- as.integer(format(mf$date, "%H"))
  mf$tod <- cut(x = mf$tod, breaks =c(-Inf, 6, 22, Inf),
                labels = c("23-07", "07-23", "23-07"))

  return(mf)

}
