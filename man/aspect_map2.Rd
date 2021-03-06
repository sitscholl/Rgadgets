% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/datasets.R
\docType{data}
\name{aspect_map2}
\alias{aspect_map2}
\title{Reclassify table for aspect values in degree using 5 classes}
\format{
A data frame with 5 rows and 3 variables:
\describe{
\item{from}{start value of aspect in degree}
\item{to}{end value of aspect in degree}
\item{name}{string representation of orientation, n = north, ne = north-east, e = east, se = south-east,
s = south, sw = south-west, w = west, nw = north-west, f = flat}
}
}
\source{
\url{https://desktop.arcgis.com/de/arcmap/10.3/tools/spatial-analyst-toolbox/how-aspect-works.htm}
}
\usage{
aspect_map2
}
\description{
A data.frame containing the range of aspect values and their corresponding exposition as a string ('north', 'south' ...)
}
\keyword{datasets}
