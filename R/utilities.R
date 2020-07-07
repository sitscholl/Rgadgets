#' Listing raster tiffs in a folder
#'
#' This function allows you to create a vector with paths to raster files in the specified folder
#' @param path a string, path to the folder with the files
#' @param pattern a regular expression. Only filenames that match pattern will be returned
#' @param full.names a  logical value. If TRUE the full path will be returned, otherwise only the file names
#' @keywords files, list
#' @export
#' @examples
#' list_rasters('a_data/original_data')

rg_list_rasters <- function(path = getwd(), pattern = '.tif$', full.names = T, ...) {

  vec <- list.files(path = path, pattern = pattern, full.names = full.names, ...)

  return(vec)
}

#' Listing csv files in a folder
#'
#' This function allows you to create a vector with paths to csv files in the specified folder
#' @param path a string, path to the folder with the files
#' @param pattern a regular expression. Only filenames that match pattern will be returned
#' @param full.names a  logical value. If TRUE the full path will be returned, otherwise only the file names
#' @keywords files, list
#' @export
#' @examples
#' list_rasters('a_data/original_data')

rg_list_csv <- function(path = getwd(), pattern = '.csv$', full.names = T, ...) {

  vec <- list.files(path = path, pattern = pattern, full.names = full.names, ...)

  return(vec)
}

#' Listing shapefiles in a folder
#'
#' This function allows you to create a vector with paths to shapefiles in the specified folder
#' @param path a string, path to the folder with the files
#' @param pattern a regular expression. Only filenames that match pattern will be returned
#' @param full.names a  logical value. If TRUE the full path will be returned, otherwise only the file names
#' @keywords files, list
#' @export
#' @examples
#' list_rasters('a_data/original_data')

rg_list_shp <- function(path = getwd(), pattern = '.shp$', full.names = T, ...) {

  vec <- list.files(path = path, pattern = pattern, full.names = full.names, ...)

  return(vec)
}

#' Remove outliers from a vector based on sigma test or interquartile range
#'
#' This function removes outliers from a vector and replaces them with an optional value based
#' on sigma test or the interquartile range
#' @param v numeric vector
#' @param type string, one of iqr for interquartile range or sigma for sigma test
#' @param fill numeric or function, either a fixed value or a function such as mean from which
#' the replacement values can be computed
#' @param range numeric, this number is multiplied with the interquartile range for type = 'iqr'
#' or the standard deviation for type = 'sigma' and determines the width of the window in which
#' values are considered as NOT being outliers
#' @param na.rm logical; if true, any NA and NaN's are removed from v before the quantiles, means or standard deviations are computed.
#' @keywords outliers, cleaning
#' @export
#' @examples
#' remove_outliers(c(1, 2, 3, 100))

rg_remove_outliers <- function(v, type = 'iqr', fill = NA, range = ifelse(type == 'iqr', 1.5, 3), na.rm = FALSE) {

  if (type == 'iqr') {

    #####IQR test for outliers
    q25 = quantile(v, c(.25), na.rm = na.rm)
    q75 = quantile(v, c(.75), na.rm = na.rm)
    iqr = q75 - q25
    upper = q75 + (iqr * range)
    lower = q25 - (iqr * range)

  } else if (type == 'sigma') {

    ####3 sigma test for outliers
    v_mean = mean(v, na.rm = na.rm)
    v_sd = sd(v, na.rm = na.rm)
    lower = v - (range * v_sd)
    upper = v + (range * v_sd)

  }

  if (is.function(fill)) {
    v_clean <- ifelse(v > upper | v < lower, fill(v), v)
  } else {
    v_clean <- ifelse(v > upper | v < lower, fill, v)
  }

  return(v_clean)

}

#' Predict to a raster layer, save it and return the path instead of the raster layer to save memory
#'
#' This takes a raster_layer or raster_stack, a model object and a character vector as filename, predicts a new raster_layer using the model object, saves it to path and returns that path
#' @param object A Raster* object. The names of the single layers have to match the variables in "model".
#' @param model A model  that is used to generate predictions from "object"
#' @param path A character vector, the predicted Raster* object will be stored under this path.
#' @param ... Further parameters passed on to raster::predict
#' @keywords model, predict, raster
#' @export

rg_predict_save <- function(object, model, path, format = 'GTiff', ...) {

  raster_pred <- raster::predict(object = object, model = model,
                                 filename = path, format = format, ...)

  print(basename(path))
  return(path)

}

#' Adds together two raster layers, saves the result under path and returns the path as character vector
#'
#' This takes two raster_layers x and y  and a character vector as filename, calculates a new raster_layer by adding raster x and raster y and then saves it to path and returns that path
#' @param x Raster layer
#' @param y Raster layer
#' @param path A character vector, the calculated Raster_layer will be stored under this path.
#' @param operation A character vector, one of +, -, * or / specifying how x and y should be combined
#' @param ... Further parameters passed on to raster::writeRaster
#' @keywords model, predict, raster
#' @export

rg_calc_save <- function(x, y, path, format = 'GTiff', operation = '+', ...) {

  if (!inherits(x, 'RasterLayer')) x <- raster::raster(x)
  if (!inherits(y, 'RasterLayer')) y <- raster::raster(y)

  if(!raster::compareRaster(x, y, orig = T)) {
    print('Reprojecting y to x using bilinear method')
    y <- projectRaster(from = y, to = x, method = 'bilinear')

  }

  raster_calc <- switch(operation,
                        '+' = x + y,
                        '-' = x - y,
                        '/' = x / y,
                        '*' = x * y)

  raster::writeRaster(x = raster_calc, filename = path, format = format, ...)

  print(basename(path))
  return(path)

}

