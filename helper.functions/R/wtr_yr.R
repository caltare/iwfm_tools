#' wtr_yr
#'
#' returns vector of water years, as integer, from vector of dates
#' @param dates vector of dates
#' @param start_month start month of water year (default=10/october)
#' @keywords 
#' @export
#' @examples
#' wtr_yr()

wtr_yr <- function(dates, start_month=10) {
  # Convert dates into POSIXlt
  dates.posix = as.POSIXlt(dates)
  # Year offset
  offset = ifelse(dates.posix$mon >= start_month - 1, 1, 0)
  # Water year
  adj.year = dates.posix$year + 1900 + offset
  # Return the water year
  as.integer(adj.year)
}