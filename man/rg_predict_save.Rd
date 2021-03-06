% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utilities.R
\name{rg_predict_save}
\alias{rg_predict_save}
\title{Predict to a raster layer, save it and return the path instead of the raster layer to save memory}
\usage{
rg_predict_save(object, model, path, format = "GTiff", ...)
}
\arguments{
\item{object}{A Raster* object. The names of the single layers have to match the variables in "model".}

\item{model}{A model  that is used to generate predictions from "object"}

\item{path}{A character vector, the predicted Raster* object will be stored under this path.}

\item{...}{Further parameters passed on to raster::predict}
}
\description{
This takes a raster_layer or raster_stack, a model object and a character vector as filename, predicts a new raster_layer using the model object, saves it to path and returns that path
}
\keyword{model,}
\keyword{predict,}
\keyword{raster}
