% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/datasets.R
\docType{data}
\name{stations_province}
\alias{stations_province}
\title{Shapefile with location of province meteorological stations}
\format{
An sf object with 119 features and 4 variables:
\describe{
\item{st_id}{id of the station}
\item{dem}{elevation of the station in meters, extracted from a dem at 25m resolution}
\item{slope}{slope of the station in degrees, extracted from a slope map at 25m resolution}
\item{aspect}{aspect of the station in degrees, extracted from an aspect map at 25m resolution}
}
}
\usage{
stations_province
}
\description{
A shapefile with point coordinates that describe the location of the province meteorological stations
}
\keyword{datasets}
