#' Digital Elevation Model of South Tyrol
#'
#' Digital elevation model for South Tyrol with a 20km buffer at 25m resolution.
#'
#' @format A raster file loaded using raster::raster()
#' @source \url{https://land.copernicus.eu/imagery-in-situ/eu-dem/eu-dem-v1-0-and-derived-products/eu-dem-v1.0}
"dem"

#' Slope raster of South Tyrol
#'
#' Slope raster for South Tyrol with a 20km buffer at 25m resolution. Calculated from dem
#' using the slope function in ArcGIS Pro. Units in degree and method set to planar.
#'
#' @format A raster file loaded using raster::raster()
#' @source \url{https://land.copernicus.eu/imagery-in-situ/eu-dem/eu-dem-v1-0-and-derived-products/eu-dem-v1.0}
"slope"

#' Aspect raster of South Tyrol
#'
#' Aspect raster for South Tyrol with a 20km buffer at 25m resolution. Calculated from dem
#' using the aspect function in ArcGIS Pro. Method set to planar.
#'
#' @format A raster file loaded using raster::raster()
#' @source \url{https://land.copernicus.eu/imagery-in-situ/eu-dem/eu-dem-v1-0-and-derived-products/eu-dem-v1.0}
"aspect"

#' Border of South Tyrol
#'
#' Polygon delineating the border or South Tyrol (Italy)
#'
#' @format An sf object loaded using st_read()
"st_border"

#' Shapefile with location of province meteorological stations
#'
#' A shapefile with point coordinates that describe the location of the province meteorological stations
#'
#' @format An sf object loaded using st_read()
"stations_province"

#' Shapefile with location of rebecka meteorological stations
#'
#' A shapefile with point coordinates that describe the location of the rebecka meteorological stations
#'
#' @format An sf object loaded using st_read()
"stations_rebecka"

#' Shapefile with location of matsch meteorological stations
#'
#' A shapefile with point coordinates that describe the location of the matsch meteorological stations
#'
#' @format An sf object loaded using st_read()
"stations_matsch"
