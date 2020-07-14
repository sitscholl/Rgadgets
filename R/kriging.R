#' Kriging of spatial data
#'
#' Tunction to perform kriging with automatic model selection using the function \code{\link[automap]{autoKrige}}.
#' Can generate continuous raster surfaces of interpolated values.
#'
#' @param frml formula that defines the dependent variable as a linear model of independent variables;
#' suppose the dependent variable has name 'z', for ordinary and simple kriging use the formula 'z~1';
#' for simple kriging also define 'beta' (see below); for universal kriging,
#' suppose 'z' is linearly dependent on 'x' and 'y', use the formula 'z~x+y'.
#' @param input_data sp object containing the data to be interpolated
#' @param pred_locations sp object containing the prediction locations. If you want to krige values onto a continuous grid
#' transform a raster to point object using \code{\link[raster]{rasterToPoints}}. See example.
#' @param raster_out logical, indicating if an interpolated raster or a table is desired as output
#' @param dem raster object, a digital elevation model, will be used in the function \code{\link[raster]{rasterize}}
#' to transfer values from pred_locations to raster cells
#' @param ... parameters passed on to \code{\link[automap]{autoKrige}}
#'
#' @return
#' @export
#'
#' @examples
#' dem <- raster::raster('dem.tif')
#' st_grd <- raster::rasterToPoints(dem, spatial = T)
#'
#' data_in <- rgdal::readOGR('points.shp')   #A shapefile containing values to be interpolated in a column called 'resid'
#'
#' result <- rg_krige(frml = resid ~ 1, input_data = data_in, pred_locations = st_grd, raster_out = T, dem = dem)

rg_krige <- function(frml, input_data, pred_locations, raster_out = F, dem = NULL, ...) {

  stopifnot(inherits(input_data, 'Spatial'))
  stopifnot(inherits(pred_locations, 'Spatial'))

  if (!raster::compareCRS(input_data, pred_locations)) {

    pred_locations <- sp::spTransform(pred_locations, raster::crs(input_data))
    print('Input spatial objects have not the same crs.
          spTransform is used to transform pred_locations to input data crs.')

  }

  if (any(is.na(input_data@data))) {

    print('NAs found in data. They will be dropped.')
    na.index <- unique(as.data.frame(which(is.na(input_data@data), arr.ind=TRUE)))[,1]
    input_data <- input_data[-na.index, ]

  }

  if (verbose == F) {

    invisible(capture.output(krige_pred <- automap::autoKrige(formula = frml,
                                                              input_data = input_data,
                                                              new_data = pred_locations,
                                                              verbose = F,
                                                              ...)))

  } else {

    krige_pred <- automap::autoKrige(formula = frml,
                                     input_data = input_data,
                                     new_data = pred_locations,
                                     ...)

  }

  if (raster_out) {

    if (is.null(dem)) stop('dem cannot be NULL if raster_out is TRUE.')

    result <-
      krige_pred$krige_output %>%
      raster::rasterize(y = dem, field = 'var1.pred', fun = mean)

  } else {

    result <-
      pred_locations@data %>%
      dplyr::bind_cols(krige_pred$krige_output@data) %>%
      dplyr::select(-c(var1.var, var1.stdev)) %>%
      dplyr::rename(krige_pred = var1.pred)
  }

  return(result)
}
