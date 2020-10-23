#' Transforms a day of year value to a date object
#'
#' @param doy integer, day of year
#' @param year integer, the year for which you want the date object
#'
#' @return
#' @export
#'
#' @examples
#' date <- rg_doyToDate(30, year = 2018)

rg_doyToDate <- function(doy, year) {

  as.Date(doy, origin = paste(year-1, "-12-31", sep = ""))

}
