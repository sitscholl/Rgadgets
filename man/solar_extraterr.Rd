% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/solar_calculations.R
\name{solar_extraterr}
\alias{solar_extraterr}
\title{Calculate extraterrestrial radiation on a horizontal plane outside the atmosphere}
\usage{
solar_extraterr(doy, G = 1361)
}
\arguments{
\item{doy}{integer, day of year}

\item{G}{numeric, Solar constant}
}
\value{
An integer or a numeric vector
}
\description{
Calculate extraterrestrial radiation on a horizontal plane outside the atmosphere
}
\examples{
solar_extraterr(doy = 100)
}
\references{
\url{https://www.e-education.psu.edu/eme810/}
}
