% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utilities.R
\name{rg_list_rasters}
\alias{rg_list_rasters}
\title{Listing raster tiffs in a folder}
\usage{
rg_list_rasters(path = getwd(), pattern = ".tif$", full.names = T, ...)
}
\arguments{
\item{path}{a string, path to the folder with the files}

\item{pattern}{a regular expression. Only filenames that match pattern will be returned}

\item{full.names}{a  logical value. If TRUE the full path will be returned, otherwise only the file names}
}
\description{
This function allows you to create a vector with paths to raster files in the specified folder
}
\examples{
list_rasters('a_data/original_data')
}
\keyword{files,}
\keyword{list}
