% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/solar_calculations.R
\name{solar_daily_pot_insol}
\alias{solar_daily_pot_insol}
\title{Calculate daily extraterrestrial insolation/irradiance for a given latitude}
\usage{
solar_daily_pot_insol(doy, lat = 0)
}
\arguments{
\item{doy}{integer, day of year. Multiple values allowed}

\item{lat}{numeric, latitude in radians! Only one value allowed}
}
\value{

}
\description{
Calculates the potential daily solar insolation/irradiance in kWh/m² for given days and latitude. Latitude has to be given in radians!
}
\examples{
daily_pot_insolation <- solar_daily_pot_insol(1:365, lat = 0.8138967)
}
\references{
\url{https://www.e-education.psu.edu/eme810/}
}
