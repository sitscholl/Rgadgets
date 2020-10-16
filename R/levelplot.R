#' Plot raster layers
#'
#' This function is a wrapper around \link[rasterVis]{levelplot-methods} with more convenient parameter names.
#'
#' @param raster raster object that should be plotted. Can be a single raster layer or a stack/brick
#' @param margin A list or a logical. If it is TRUE, two marginal graphics show the column (x) and row (y) summaries of the Raster* object. The summary is computed with the function mean. If it is a list, it contains parameters in ‘name=value’ form that define both margins, and may contain two other lists called ‘x’ and ‘y’ whose components affect the respective margins only.
#' @param layout vector to specify number of columns and rows in the form c(ncol, nrow)
#' @param palette string, the name of the palette or a function that returns color names such as RColorBrewer::brewer.pal(9, 'RdBu). If the name of the palette is given must be one of 'RdBu', 'BuRd', 'Magma', 'Viridis', 'Inferno'.
#' @param breaks numeric, locations at which the colors change. Do not changes the legend labels, for this use legend.labels and legend.labels.position
#' @param nbreaks numeric, number of breaks. Only used when breaks = NULL
#' @param xlab string, Labels of the x axis. Supports also a vector of strings to draw multiple labels
#' @param ylab string, Labels of the y axis. Supports also a vector of strings to draw multiple labels
#' @param axis.labels.color string, color of the axis labels. Changes both x and y labels
#' @param axis.labels.x.color string, color of the x labels
#' @param axis.labels.y.color string, color of the y labels
#' @param axis.labels.size numeric, size of the axis labels. Changes both x and y labels
#' @param axis.labels.x.size numeric, size of the x labels
#' @param axis.labels.y.size numeric, size of the y labels
#' @param axis.labels.angle numeric, angle of the axis labels. Changes both x and y labels
#' @param axis.labels.x.angle numeric, angle of the x labels
#' @param axis.labels.y.angle numeric, angle of the y labels
#' @param axis.labels.font string, fontface of the axis labels. Changes both x and y labels
#' @param axis.labels.x.font string, fontface of x labels
#' @param axis.labels.y.font string, fontface of y labels
#' @param axis.ticks logical, should x and y axis ticks be displayed?
#' @param axis.ticks.x logical, should x axis ticks be displayed?
#' @param axis.ticks.y logical, should y axis ticks be displayed?
#' @param axis.line.col string, color of the box around the plot
#' @param legend.labels string or numeric, the labels at the side of the legend. Set to legend.labels = breaks to have the same labels as breaks
#' @param legend.label.position numeric, the position of the legend labels along the legend. Set to legend.label.position = breaks to have the same positions as specified in breaks
#' @param legend.label.size numeric, size of the legend labels
#' @param legend.position string, position of the legend. One of 'top', 'right', 'bottom' or 'left'. Legend.title seems not to work with legend.position = 'right'
#' @param legend.line.color string, color of the legend frame
#' @param legend.width numeric, width of the legend
#' @param legend.height numeric, height of the legend
#' @param legend.title string, title of the legend. Seems not to work when legend.position = 'right'
#' @param plot.background.col string, background color of the plot
#' @param plot.title string, title of the plot
#' @param plot.title.size numeric, size of the plot title
#' @param plot.title.color string, color of the plot title
#' @param strip.text string, vector of titles of plot strips. Length must be equal to the number of plot strips
#' @param strip.text.size numeric, size of strip text
#' @param strip.text.color string, color of strip text
#' @param strip.background.col string, background color of the box around strip title
#' @param strip.border.col string, color of the box around strip title
#' @param ... further parameters passed on to \link[rasterVis]{levelplot-methods}
#'
#' @return
#' @export
#'
#' @examples
#' dem <- Rgadgets::rg_dem()
#'
#' rg_levelplot(dem, palette = 'Inferno', xlab = c('X', 'test', 'test'), ylab = 'Y',
#' plot.background.col = 'gray70', plot.title = 'A new plot fn', plot.title.size = 3,
#' axis.line.col = 'black', strip.background.col = 'gray70', strip.border.col = 'red',
#' strip.text = c('test'), strip.text.size = 2, strip.text.color = 'white', legend.title = 'legend title',
#' legend.position = 'left', nbreaks = 10, axis.ticks.y = F, plot.title.color = 'red',
#' axis.labels.angle = 30, axis.labels.font = 'bold', axis.labels.color = 'yellow',
#' axis.labels.size = 3)
rg_levelplot <- function(raster,
                         margin = FALSE,
                         layout = NULL,
                         palette = viridis::viridis(10),

                         breaks = NULL,
                         nbreaks = 5,

                         xlab = NULL,
                         ylab = NULL,
                         axis.labels.color = NULL,
                         axis.labels.x.color = 'black',
                         axis.labels.y.color = 'black',
                         axis.labels.size = NULL,
                         axis.labels.x.size = 1,
                         axis.labels.y.size = 1,
                         axis.labels.angle = NULL,
                         axis.labels.x.angle = 0,
                         axis.labels.y.angle = 0,
                         axis.labels.font = NULL,
                         axis.labels.x.font = 'plain',
                         axis.labels.y.font = 'plain',

                         axis.ticks = NULL,
                         axis.ticks.x = T,
                         axis.ticks.y = T,

                         axis.line.col = 'transparent',

                         legend.labels = NULL,
                         legend.label.position = NULL,
                         legend.label.size = 1,
                         legend.position = 'left',
                         legend.line.color = 'black',
                         legend.width = 1,
                         legend.height = 1,
                         legend.title = NULL,

                         plot.background.col = 'transparent',
                         plot.title = NULL,
                         plot.title.size = 1,
                         plot.title.color = 'black',

                         strip.text = names(raster),
                         strip.text.size = 1,
                         strip.text.color = 'black',
                         strip.background.col = 'transparent',
                         strip.border.col = 'transparent',

                         ...) {

  #include terra raster objects

  #add option for north arrow and scale bar
  #https://oscarperpinan.github.io/rastervis/FAQ.html

  #add parameters to customize axis.ticks. See ?lattice::xyplot for details

  #add customization of legend title. Example:
  # levelplot(r, margin = FALSE,
  #           colorkey = list(title = "[m]",
  #                           title.gpar = list(cex = 1,
  #                                             font = 2,
  #                                             col = 'red')
  #           ))

  #get palette if string is given as input
  if(is.character(palette) & length(palette) == 1) {
    palette <- switch (palette,
                       'RdBu' = RColorBrewer::brewer.pal(9, 'RdBu'),
                       'BuRd' = rev(RColorBrewer::brewer.pal(9, 'RdBu')),
                       'Viridis' = viridis::viridis(10),
                       'Magma' = viridis::magma(10),
                       'Inferno' = viridis::inferno(10)
    )
  }

  #construct breaks if none given
  if(is.null(breaks)) {
    breaks <- seq(min(minValue(raster)),
                  max(maxValue(raster)),
                  length.out = nbreaks)
    breaks <- round(breaks, -1)
  }

  #set axis ticks
  if(!is.null(axis.ticks)) {
    axis.ticks.x <- axis.ticks
    axis.ticks.y <- axis.ticks
  }

  #set axix.label angles
  if(!is.null(axis.labels.angle)) {
    axis.labels.x.angle <- axis.labels.angle
    axis.labels.y.angle <- axis.labels.angle
  }

  #set axis.labels font
  if(!is.null(axis.labels.font)) {
    axis.labels.x.font <- axis.labels.font
    axis.labels.y.font <- axis.labels.font
  }

  #set axis.labels font
  if(!is.null(axis.labels.color)) {
    axis.labels.x.color <- axis.labels.color
    axis.labels.y.color <- axis.labels.color
  }

  #set axis.labels font
  if(!is.null(axis.labels.size)) {
    axis.labels.x.size <- axis.labels.size
    axis.labels.y.size <- axis.labels.size
  }

  #adjust theme configuration
  theme <- rasterVis::rasterTheme(region = palette,
                                  strip.background = list(col = strip.background.col),
                                  strip.border = list(col = strip.border.col),
                                  axis.line = list(col = axis.line.col),
                                  panel.background = list(col = plot.background.col))

  #adjust colorkey (=legend)
  colorkey <- list(at = breaks, #legend breaks
                   space = legend.position, #legend position
                   axis.line = list(col = legend.line.color), #legend frame

                   width = legend.width,
                   height = legend.height,

                   #customize legend labels
                   labels = list(cex = legend.label.size,
                                 at = legend.label.position,
                                 labels = legend.labels),
                   title = legend.title
                   )

  #adjust axis ticks
  axis_ticks <- list(x = list(draw = axis.ticks.x),
                     y = list(draw = axis.ticks.y))

  plot <- rasterVis::levelplot(raster,
                               layout = layout,

                               #breaks
                               at = breaks,

                               #strip text
                               names.attr = strip.text,
                               par.strip.text = list(cex = strip.text.size,
                                                     col = strip.text.color),

                               #plot title and labels
                               main = list(label = plot.title,
                                           cex = plot.title.size,
                                           col = plot.title.color),

                               xlab = list(xlab,
                                           rot = axis.labels.x.angle,
                                           fontface = axis.labels.x.font,
                                           col = axis.labels.x.color,
                                           cex = axis.labels.x.size),
                               ylab = list(ylab,
                                           rot = axis.labels.y.angle,
                                           fontface = axis.labels.y.font,
                                           col = axis.labels.y.color,
                                           cex = axis.labels.y.size),

                               #outer margins
                               margin = margin,

                               #theme settings
                               par.settings = theme,

                               #legend settings
                               colorkey = colorkey,

                               #axis ticks
                               scales = axis_ticks)

  #latticeExtra::layer(sp.lines(as_Spatial(st_border), lwd = 1))

  return(plot)

}
