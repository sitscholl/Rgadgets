#' Calculate extraterrestrial radiation on a horizontal plane outside the atmosphere
#'
#' @param doy integer, day of year
#' @param G numeric, Solar constant
#'
#' @return An integer or a numeric vector
#' @export
#' @references \url{https://www.e-education.psu.edu/eme810/}
#'
#' @examples
#' solar_extraterr(doy = 100)

solar_extraterr <- function(doy, G = 1361) {

  #pi/180 converts to radians
  G * ( 1 + 0.033 * cos( ((360/365) * (pi/180)) * doy ))

}

#' Calculate solar declination
#' @description Calculates the solar declination for the given days. The angle is returned in radians, because the trigonometric R functions (sin, cos, tan) are workign in radians.
#' The Fourier series method is used to calculate solar declination.
#'
#' @param doy  integer, day of year
#'
#' @return An integer or a numeric vector in radians
#' @export
#' @references \url{https://www.e-education.psu.edu/eme810/}
#'
#' @examples
#' solar_declination(doy = 100)

solar_declination <- function(doy) {

  #pi/180 converts to radians
  B <- (doy - 1) * ((360 / 365) * (pi/180))

  dec_angle <- (180/pi) * (0.006918 - 0.399912 * cos(B) + 0.070257 * sin(B) - 0.006758 * cos(2*B) + 0.000907 * sin(2*B) - 0.002679 * cos(3*B) + 0.00148 * sin(3*B))

  dec_angle <- dec_angle * (pi/180)

  return(dec_angle)
}

#' Calculate solar sunset angles for a given latitude
#' @description Calculates solar sunset angles for a given latitude. The input values have to be in radians! Convert from degree to radians by multiplication with pi/180
#' As default a latitude value of 0 is used.
#'
#' @param lat numeric, latitude in radians. Only a single value allowed
#' @param dec numeric, solar declination values in radians. One or multiple values allowed
#'
#' @return
#' @export
#' @references \url{https://www.e-education.psu.edu/eme810/}
#'
#' @examples
#' declination <- solar_declination(1:365)
#' angle <- solar_sunset_angle(dec = declination, lat = 0.8138967)
solar_sunset_angle <- function(dec, lat = 0) {

  stopifnot(length(lat) == 1)

  sunset_angle <- acos(-tan(lat) * tan(dec))

  #convert to radians
  sunset_angle <- sunset_angle * (pi/180)

  return(sunset_angle)

}

#' Calculate daily extraterrestrial insolation/irradiance for a given latitude
#' @description Calculates the potential daily solar insolation/irradiance in kWh/m² for given days and latitude. Latitude has to be given in radians!
#'
#' @param doy integer, day of year. Multiple values allowed
#' @param lat numeric, latitude in radians! Only one value allowed
#'
#' @return
#' @export
#' @references \url{https://www.e-education.psu.edu/eme810/}
#'
#' @examples
#' daily_pot_insolation <- solar_daily_pot_insol(1:365, lat = 0.8138967)

solar_daily_pot_insol <- function(doy, lat = 0) {

  stopifnot(length(lat) == 1)

  extraterr_radiation <- Rgadgets::solar_extraterr(doy = doy)

  declination <- Rgadgets::solar_declination(doy = doy)

  sunset_angle <- Rgadgets::solar_sunset_angle(dec = declination, lat = lat)

  pot_insol <- ( (12 * 3600) / pi ) * extraterr_radiation * (2 * ( ((pi/180) * sunset_angle * sin(lat) * sin(declination)) +
                                                                     (cos(lat) * cos(declination) * sin(sunset_angle)) ))

  pot_insol_kWh <- pot_insol/1000

  return(pot_insol_kWh)
}
