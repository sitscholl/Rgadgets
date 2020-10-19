#' Bivariate plotting of raster maps
#'
#' These functions can be used to combine two raster layers in a bivariate plot. To generate a
#' bivariate plot, first a matrix with the color codes and a raster layer that is a combination of
#' the two layers that should be plotted have to be calculated. The function \code{rg_big_cmat} can be used
#' to generate the color matrix and the function \code{rg_biv_create_raster} to combine the two single
#' raster files. The outputs of these two functions are then used in the function \code{rg_biv_plot_raster}
#' to create the final plot. The function \code{rg_biv_get_legend} can optionally be used to inspect
#' the final legend and is used internally by the function \code{rg_biv_plot_raster} to create the legend
#' for the plot.
#'
#' @param breaks integer, number of breaks used to generate the bivariate color scale
#' @param style integer, number between 1 and 9 to select different predefined color combinations
#' @param cmat matrix, a matrix with hexcolor codes such as returned from the function \code{rg_biv_cmat}
#' @param x raster layer, first layer for bivariate plot
#' @param y raster layer, second layer for bivariate plot
#' @param bivraster raster, the raster object returned from the function \code{rg_biv_create_raster}
#' @param upper.left string, upper left color
#' @param upper.right string, upper right color
#' @param lower.left string, lower left color
#' @param lower.right string, lower right color
#' @param xlab string, x-label of the legend
#' @param ylab string, y-label of the legend
#' @param border sf object, optionally plot an sf object as border around raster. Make sure the sf object has the same crs as the bivraster
#' @param crs string, specification of the coordinate system you want to plot the bivraster in. Per Default uses EPSG:4326
#' @param na.value string, color for NA values
#' @param legend.height numeric, height of legend
#' @param legend.width numeric, width of legend
#' @param legend.x numeric, relative x position of legend
#' @param legnend.y numeric, relative y position of legend
#' @param limits optionally zoom in into plot using x and y limits. Use the syntax limits = c(xmin, xmax, ymin, ymax).
#' @param label.size numeric, size of legend labels
#' @param label.color string, color of legend label
#'
#' @source \url{#https://stackoverflow.com/questions/54144269/bivariate-choropleth-map-in-r}
#'
#' @examples
#' breaks <- 3
#' cmat <- rg_biv_cmat(breaks, style = 1)
#' legend <- rg_biv_get_legend(cmat, xlab = 'Gain', ylab = 'Loss')
#' xy <- rg_biv_create_raster(x, y, breaks)
#' map <- rg_biv_plot_raster(xy, cmat, border = st_border_proj, xlab = 'Gain', ylab = 'Loss', limits = c(10.7, 12, 46.2, 46.8))
#' map


#' @describeIn rg_biv_cmat Generate color matrix
#' @export

rg_biv_cmat <- function(breaks, style = 1,
                        upper.left = NULL, upper.right = NULL,
                        lower.left = NULL, lower.right = NULL) {

  # Define edge colors
  # Style 1-5 from http://www.joshuastevens.net/cartography/make-a-bivariate-choropleth-map/
  if(style == 1) {
    upperleft = "#64ACBE"
    upperright = "#574249"
    lowerleft = "#E8E8E8"
    lowerright = "#C85A5A"
  } else if (style == 2) {
    upperleft = "#BE64AC"
    upperright = "#3B4994"
    lowerleft = "#E8E8E8"
    lowerright = "#5AC8C8"
  } else if (style == 3) {
    upperleft = "#73AE80"
    upperright = "#2A5A5B"
    lowerleft = "#E8E8E8"
    lowerright = "#6C83B5"
  } else if (style == 4) {
    upperleft = "#9972AF"
    upperright = "#804D36"
    lowerleft = "#E8E8E8"
    lowerright = "#C8B35A"
  } else if (style == 5) {
    upperleft = "#DA8DC8"
    upperright = "#697AA2"
    lowerleft = "#E8E8E8"
    lowerright = "#73BCA0"
  } else if (style == 6) {
    # Similar to Teuling, Stockli, Seneviratnea (2011) [https://doi.org/10.1002/joc.2153]
    upperleft = "#F7900A"
    upperright = "#993A65"
    lowerleft = "#44B360"
    lowerright = "#3A88B5"
  } else if (style == 7) {
    # Viridis style
    upperleft = "#FEF287"
    upperright = "#21908D"
    lowerleft = "#E8F4F3"
    lowerright = "#9874A1"
  } else if (style == 8) {
    # Similar to Fjeldsa, Bowie, Rahbek 2012
    upperleft = "#34C21B"
    upperright = "#FFFFFF"
    lowerleft = "#595757"
    lowerright = "#A874B8"
  } else if (style == 9) {
    # Default from original source
    upperleft = "#0096EB"
    upperright = "#820050"
    lowerleft= "#BEBEBE"
    lowerright = "#FFE60F"
  }

  #replace edge colors with user given values
  if(!is.null(upper.left)) upperleft <- upper.left
  if(!is.null(lower.left)) lowerleft <- lower.left
  if(!is.null(upper.right)) upperright <- upper.right
  if(!is.null(lower.right)) lowerright <- lower.right

  #create empty matrix
  m <- matrix(ncol=breaks, nrow=breaks)

  #create color scale for given breaks
  b <- breaks-1
  b <- (0:b)/b
  col1 <- grDevices::rgb(grDevices::colorRamp(c(upperleft, lowerleft))(b), max=255)
  col2 <- grDevices::rgb(grDevices::colorRamp(c(upperright, lowerright))(b), max=255)
  cm <- apply(cbind(col1, col2), 1, function(i) grDevices::rgb(grDevices::colorRamp(i)(b), max=255))

  #fill matrix with color values
  return(cm[, ncol(cm):1 ])
}

#' @describeIn rg_biv_cmat Generate legend from color matrix
#' @export

rg_biv_get_legend <- function(cmat, xlab="", ylab="",
                              label.size = 12, label.color = 'black') {

  stopifnot(nrow(cmat) == ncol(cmat))
  breaks <- nrow(cmat)

  # Transform matrix to data.frame
  legend_df <-
    cmat %>%
    as.data.frame() %>%
    tidyr::pivot_longer(cols = tidyselect::everything()) %>%
    dplyr::mutate(y = rep(1:breaks, times = breaks),
                  x = rep(1:breaks, each = breaks))

  # Create ggplot object
  legend_plot <-
    legend_df %>%
    ggplot2::ggplot(aes(x, y, fill = value)) +
    ggplot2::geom_raster() +

    ggplot2::scale_fill_identity() +

    ggplot2::coord_equal(expand = FALSE) +
    ggplot2::theme_void() +
    ggplot2::theme(aspect.ratio = 1,
                   axis.title = ggplot2::element_text(size = label.size,
                                                      colour = label.color,
                                                      hjust = 0.5,
                                                      vjust = 1),
                   axis.title.y = ggplot2::element_text(angle = 90,
                                                        hjust = 0.5)) +
    ggplot2::xlab(bquote(.(xlab) ~  symbol("\256"))) +
    ggplot2::ylab(bquote(.(ylab) ~  symbol("\256")))

  return(legend_plot)
}

#' @describeIn rg_biv_cmat Combine both single raster files to bivariate raster
#' @export

rg_biv_create_raster <- function(x, y, breaks) {

  #Create quantiles of raster values
  q1 <- raster::quantile(x, seq(0,1,1/(breaks)))
  q2 <- raster::quantile(y, seq(0,1,1/(breaks)))

  #Reclassify raster values to quantiles
  r1 <- raster::cut(x, q1, include.lowest=TRUE)
  r2 <- raster::cut(y, q2, include.lowest=TRUE)

  #Calculate rasterCM
  rasterCM <- raster::overlay(r1, r2, fun=function(i, j) {
    (j-1) * breaks + i
  })

  return(rasterCM)
}

#' @describeIn rg_biv_cmat Create bivariate plot
#' @export

rg_biv_plot_raster <- function(bivraster, cmat, xlab = '', ylab = '', border = NULL,
                               crs = "+init=epsg:4326", na.value = 'transparent',
                               legend.height = .25,
                               legend.width = .25,
                               legend.x = .75,
                               legend.y = .2,
                               limits = NULL) {

  # Transform raster to data.frame
  r_df <-
    bivraster %>%
    raster::projectRaster(crs = crs) %>%
    raster::as.data.frame(xy = TRUE) %>%
    tibble::as_tibble() %>%
    dplyr::rename("BivValue" = 3) %>%
    tidyr::pivot_longer(names_to = "Variable",
                        values_to = "bivVal",
                        cols = BivValue)

  # Create ggplot without legend
  map <-
    ggplot2::ggplot() +
    ggplot2::geom_raster(data = r_df, aes(fill = bivVal, x = x, y = y)) +

    ggplot2::scale_fill_gradientn(colours = cmat, na.value = na.value) +

    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "none") +
    ggplot2::labs(x = "Longitude", y = "Latitude")

  if(!is.null(border)) map <- map + ggplot2::geom_sf(data = border, fill = NA)
  if(!is.null(limits)) map <- map  + ggplot2::coord_sf(xlim = c(limits[[1]], limits[[2]]),
                                                       ylim = c(limits[[3]], limits[[4]]),
                                                       expand = FALSE)

  # Overlay the legend on the map
  #create legend as ggplot object
  legend <- Rgadgets::rg_biv_get_legend(cmat, xlab = xlab, ylab = ylab)
  fig <-
    map %>%
    cowplot::ggdraw() +
    cowplot::draw_plot(legend + ggplot2::theme(plot.background = ggplot2::element_rect(fill = "white", colour = NA)),
                       width = legend.width,
                       height = legend.height,
                       x = legend.x,
                       y = legend.y)

  return(fig)

}
