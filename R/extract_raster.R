#' Extracting values from one or multiple raster files at point locations
#'
#' This function allows you to extract the values of one or many rasters at the location of given points.
#' @param rasters An object of class RasterLayer, RasterStack or RasterBrick. Can also be a path to a raster file or multiple paths. In the last case the corresponding files will be opened in a stack.
#' @param shp A SpatialPointsDataFrame with the location of the points where the values should be extracted.
#' @param id.vars A vector with the column names of 'shp' that should be in the resulting data.frame
#' @param value.name Name of the column that stores the extracted values. Defaults to 'value'.
#' @param variable.name Nae of the column that stores the names of the input rasters. Defaults to 'variable'.
#' @return A data.frame with the first column corresponding to the first column in 'shp' and two columns that contain the names of the given rasters and the extracted values, respectively.
#' @keywords raster extraction
#' @export
#' @examples
#' extract.raster(rasters = data/dem_st.tif,
#'   shp = station_sp,
#'   id.vars = c('st_id', 'st_group'),
#'   value.name = value
#'   variable.name = variable)

rg_extract_raster <- function(rasters, shp, id.vars,
                              value.name = 'value',
                              variable.name = 'variable', ...) {

  if (class(rasters) == 'character') {
    #Open the rasters either in a stack or as a single file
    if (length(rasters) > 1) {
      raster.open <- raster::stack(rasters)
    } else {
      raster.open <- raster::raster(rasters)
    }
  } else if ('RasterLayer' %in% class(rasters) | 'RasterStack' %in% class(rasters) | 'RasterBrick' %in% class(rasters)) {
    raster.open <- rasters
  } else {
    stop('Input rasters are not of class character or RasterLayer or Raster Stack!')
  }

  if (!raster::compareCRS(raster.open, shp)) {
    print('Reprojecting shapefile to coordinate system of input raster')
    shp <- sp::spTransform(shp, raster::crs(raster.open))
  }

  #Extract the values from the opened rasters
  extract.df <- raster::extract(raster.open,
                                shp,
                                sp = T,
                                ...)

  #Transform to a usable dataframe
  cols.to.use <- colnames(extract.df@data)[stringr::str_detect(colnames(extract.df@data),
                                                               stringr::str_c(c(id.vars, names(raster.open)),
                                                                              collapse = '|'))]

  extract.df <- extract.df@data[ ,cols.to.use]

  #Remove all rows where for all points the value is na
  extract.df <- extract.df[rowSums(is.na(extract.df[names(raster.open)])) != length(names(raster.open)), ]

  #Melt the dataframe
  extract.df <- reshape2::melt(extract.df,
                               id.vars = colnames(extract.df)[!colnames(extract.df) %in% names(raster.open)],
                               value.name = value.name,
                               variable.name = variable.name)

  extract.df

}
