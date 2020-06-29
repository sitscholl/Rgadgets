#' Transforming the spatial reference of sp or sf objects to be plotted via leaflet. Transforms the
#' spatial reference to the WGS84 projection (epsg:4326).
#'
#' @param object Sp or sf object
#' @keywords  leflet, transform, spatial reference
#' @export
#' @examples
#'
#' points <- leaflet_transform(station_shp)

leaflet_transform <- function(object) {

  if (any(inherits(object, 'SpatialPointsDataFrame'),
          inherits(object, 'SpatialLinesDataFrame'),
          inherits(object, 'SpatialPolygonsDataFrame'))) {

    object_out <- sp::spTransform(object, sp::CRS("+init=epsg:4326"))

  } else if(inherits(object, 'sf')) {

    object_out <- sf::st_transform(object, sp::CRS("+init=epsg:4326"))

  }

  return(object_out)

}
