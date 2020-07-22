#' Classify aspect values in degree to aspect classes like south, north, northwest...
#'
#' Uses the table from \code{\link{aspect_map}} for complex and \code{\link{aspect_map2}} for the simple reclassification.
#' @param x vector or raster object that should be reclassified. Aspect values have to be in degree between 0 and 360
#' @param method string, simple for reclassification using the five classes south, north, west, east and flat or complex
#' to also use classes like northwest, southeast...
#' @param ... further arguments passed on to \code{\link[raster]{reclassify}} or \code{\link[base]{cut}}
#'
#' @return reclassified dataset with the same format as x
#' @seealso \code{\link{aspect_map}}; \code{\link{aspect_map2}}
#' @export
#'
#' @examples
#' rg_classify_aspect(0:365, method = 'complex')

rg_classify_aspect <- function(x, method = 'simple', ...) {

  if (!method %in% c('simple', 'complex')) stop('Method has to be one of "simple" or "complex"')

  aspect_table <- switch (method,
                          'simple' = Rgadgets::aspect_map2,
                          'complex' = Rgadgets::aspect_map
  )

  if (inherits(x, 'RasterLayer')) {
    aspect_table_re <-
      aspect_table %>%
      group_by(name) %>%
      mutate(name = cur_group_id()) %>%
      ungroup()

    reclass <- raster::reclassify(x, as.matrix(aspect_table_re), ...)

  } else if (inherits(x, "numeric") | inherits(x, "integer")) {

    reclass <- cut(x,
                   breaks = unique(c(aspect_table$from, aspect_table$to)),
                   labels = aspect_table$name,
                   ...)

  } else {
    stop('Input variable x inherits neither RasterLayer, numeric nor integer. Please use one of these input formats.')
  }

  return(reclass)

}
