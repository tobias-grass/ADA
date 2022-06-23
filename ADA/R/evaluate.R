#' Generates evaluation statistic for data frame
#'
#' @description creates a statistic containing \code{mean}, \code{sd} etc. of the
#' data frame provided. The data frame must be formatted with [ADA::formatting()]
#' beforehand. Depending on the number of levels in season and time of day the size
#' of the resulting statistic can vary.
#'
#' @param data_frame a data frame in specific form [ADA::formatting()]
#' @param formula a formula describing the specific layout of the data frame
#' @param path a path with file name for the generated .csv file. If no path but
#' name is provided the .csv file will be generated in the current working
#' directory
#' @param write determines if .csv file should be generated
#'
#' @return returns summary statistic in convenient form
#'
#' @seealso [ADA::formatting()], [base::data.frame()]
#' @export
#'
evaluate <- function(data_frame, formula, path, write = TRUE) {

  #---------------------------------------------------------
  # sanity checks
  #---------------------------------------------------------

  stopifnot(is.data.frame(data_frame))
  stopifnot(inherits(formula, "formula"))
  stopifnot(is.character(path), length(path) == 1L)
  stopifnot(is.logical(write))

  #---------------------------------------------------------
  # function body
  #---------------------------------------------------------

  #split model frame in all three main sections (poll, param, date) and extent
  #previous formula with season and time of day (tod) parameter
  formula = Formula(formula)
  formula = update(formula, . ~ . + season + tod | date)

  mfl = model.part(object = formula, data = data_frame, lhs=1)

  #get all the different pollutant names according to the formula provided
  pollutant_id = names(mfl)

  #store statistics in data frame and rbind everything together
  result = data.frame()

  for (p in pollutant_id) {

    tmp4 = subset(data_frame, select = c(p, "date"))

    result = rbind(result, statistics(tmp4))

    for (s in unique(data_frame$season)) {

      tmp3 = subset(data_frame, subset = season == s, select = c(p, "date"))

      result = rbind(result, statistics(tmp3, sefason = s))

      for (t in unique(data_frame$tod)) {

        tmp = subset(data_frame, subset = season == s & tod == t,
                     select = c(p, "date"))

        result = rbind(result, statistics(tmp, season = s, tod = t))
      }
    }

    for (d in unique(data_frame$tod)) {

      tmp2 = subset(data_frame, subset = tod == d,
                   select = c(p, "date"))

      result = rbind(result, statistics(tmp2, tod = d))
    }
  }

  #round numerical values to three digits
  numeric_columns = sapply(result, class) == "numeric"
  result[numeric_columns] = round(result[numeric_columns], 3)

  #generate .csv file if desired in working directory if no path is given
  if (write) {
    write.csv2(result, path)
  }

  return(result)

}
