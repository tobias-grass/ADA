time_statistics <- function(x) {

    #---------------------------------------------------------
    # sanity checks
    #---------------------------------------------------------

    #stopifnot(is.vector(x, mode = "numeric"))

    #---------------------------------------------------------
    # function body
    #---------------------------------------------------------

    #x = na.omit(x)
    min_time = min(x)
    max_time = max(x)
    typ_SI = (as.numeric(max_time) - as.numeric(min_time))/length(x)

    val = data.frame(min_time,
                     max_time,
                     typ_SI)

    return(val)
}
