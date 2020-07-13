#' Digital Elevation Model of South Tyrol
#'
#' Digital elevation models for South Tyrol with resolutions at 25, 100 and 250m. Lower resolution dems
#' have been calculated from the dem at 25m resolution using the ArcGIS "resample" tool with bilinear interpolation.
#'
#' @format A raster file loaded using raster::raster()
#' @source \url{https://land.copernicus.eu/imagery-in-situ/eu-dem/eu-dem-v1-0-and-derived-products/eu-dem-v1.0}
"rg_dem"

#' #' Slope raster of South Tyrol
#' #'
#' #' Slope raster for South Tyrol with a 10km buffer at 25m resolution. Calculated from dem
#' #' using the slope function in ArcGIS Pro. Units in degree and method set to planar.
#' #'
#' #' @format A raster file loaded using raster::raster()
#' #' @source \url{https://land.copernicus.eu/imagery-in-situ/eu-dem/eu-dem-v1-0-and-derived-products/eu-dem-v1.0}
#' "rg_slope"
#'
#' #' Aspect raster of South Tyrol
#' #'
#' #' Aspect raster for South Tyrol with a 10km buffer at 25m resolution. Calculated from dem
#' #' using the aspect function in ArcGIS Pro. Method set to planar.
#' #'
#' #' @format A raster file loaded using raster::raster()
#' #' @source \url{https://land.copernicus.eu/imagery-in-situ/eu-dem/eu-dem-v1-0-and-derived-products/eu-dem-v1.0}
#' "rg_aspect"

#' Reclassify table for aspect values in degree using 10 classes
#'
#' A data.frame containing the range of aspect values and their corresponding exposition as a string ('north', 'south' ...)
#'
#' @format A data frame with 10 rows and 3 variables:
#' \describe{
#' \item{from}{start value of aspect in degree}
#' \item{to}{end value of aspect in degree}
#' \item{name}{string representation of orientation, n = north, ne = north-east, e = east, se = south-east,
#' s = south, sw = south-west, w = west, nw = north-west, f = flat}
#' }
#'
#' @source \url{https://desktop.arcgis.com/de/arcmap/10.3/tools/spatial-analyst-toolbox/how-aspect-works.htm}
"aspect_map"

#' Reclassify table for aspect values in degree using 5 classes
#'
#' A data.frame containing the range of aspect values and their corresponding exposition as a string ('north', 'south' ...)
#'
#' @format A data frame with 5 rows and 3 variables:
#' \describe{
#' \item{from}{start value of aspect in degree}
#' \item{to}{end value of aspect in degree}
#' \item{name}{string representation of orientation, n = north, ne = north-east, e = east, se = south-east,
#' s = south, sw = south-west, w = west, nw = north-west, f = flat}
#' }
#'
#' @source \url{https://desktop.arcgis.com/de/arcmap/10.3/tools/spatial-analyst-toolbox/how-aspect-works.htm}
"aspect_map2"

#' Border of South Tyrol
#'
#' Polygon delineating the border or South Tyrol (Italy)
#'
#' @format An sf object loaded using st_read()
"st_border"

#' Border of Municipalities in South Tyrol
#'
#' Polygon delineating the border of the municipalities in South Tyrol (Italy)
#'
#' @format An sf object loaded using st_read()
"st_municipalities"

#' Border of the districts in South Tyrol
#'
#' Polygon delineating the border of the districts in South Tyrol (Italy)
#'
#' @format An sf object loaded using st_read()
"st_districts"

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
