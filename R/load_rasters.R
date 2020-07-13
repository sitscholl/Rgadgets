#' Load digital elevation model from Rgadgets package
#'
#' @param res integer, resolution of resulting raser file in meters. Possible values include 25, 100 and 250
#'
#' @return a raster file
#' @seealso \code{\link{rg_dem}}; \code{\link{rg_dem100}}; \code{\link{rg_dem250}}
#' @export
#'
#' @examples
#' dem <- rg_dem()
rg_dem <- function(res = 25) {

  r_name <- switch (res,
    25 = 'dem.tif',
    100 = 'dem100.tif',
    250 = 'dem250.tif'
  )

  return(raster::raster(system.file("extdata", r_name, package = "Rgadgets")))
}
