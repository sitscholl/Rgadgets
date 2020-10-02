#' Adding model equation to ggplot. Wrapper around \link[ggpmisc]{stat_poly_eq} with standard options for label
#'
#' @param formula Formula, Model formula
#' @param elements String or vector of strings, The parts of the model statistics that should be included in the output
#' @param sep String, separator between model elements
#' @param ... Further parameters passed on to \link[ggpmisc]{stat_poly_eq}
#' one of index or name.
#' @keywords ggplot, model, equation
#' @export
#' @examples
#'
#'

rg_geom_equation <- function(formula, elements = c('eq', 'r2', 'p'),
                             sep = ', ', ...) {

  possible_elements <- c('eq', 'r2', 'p', 'aic', 'bic')
  if (!any(elements %in% possible_elements)) {
    stop(paste0('Unknown string in elements. Possible strings are: ',
                paste(possible_elements, collapse = ', ')))
  }

  switch_fn <- function(s) {

    s_new <-switch (s,
                    'eq' = 'eq.label',
                    'r2' = 'adj.rr.label',
                    'p' = 'p.value.label',
                    'aic' = 'AIC.label',
                    'bic' = 'BIC.label'
    )

  }

  elements_list <- lapply(elements, switch_fn)

  ggpmisc::stat_poly_eq(parse = T,
                        formula = formula,
                        aes(label = stat(paste(!!!unlist(syms(elements_list)),
                                               sep = sprintf("*\"%s \"*", sep)))),
                        ...)

}

#' Plot bars with cumstom design
#'
#' Wrapper around \code{geom_bar()} with default parameters to modify the design of the bars. For a description
#' of the function parameters see \link[ggplot2]{geom_bar}
#'
#' @param mapping aestetic mapping
#' @param stat string, how to calculate height of bars?
#' @param width integer, width of the bars
#' @param position position of the bars
#' @param color color of the frame around the bars
#' @param ... parameters passed on to \code{geom_bar()}
#'
#' @return
#' @export
#'
#' @examples
#' ggplot(BOD, aes(factor(Time), demand)) +
#' rg_geom_bar(aes(fill = factor(Time)))

rg_geom_bar <- function(mapping = NULL, stat = 'identity', width = .4,
                        position = ggplot2::position_dodge(width = .5),
                        color = 'black', ...) {
  ggplot2::geom_bar(mapping = mapping,
                    stat = stat,
                    width = width,
                    position = position,
                    color = color, ...)
}

#' Plot points with cumstom design
#'
#' Wrapper around \code{geom_point()} with default parameters to modify the design of the points. For a description
#' of the function parameters see \link[ggplot2]{geom_point}
#'
#' @param mapping aestetic mapping
#' @param pch integer, point shape
#' @param color string, color of point
#' @param size integer, point size
#' @param stroke integer, width of point border
#' @param ... parameters passed on to \code{geom_point()}
#'
#' @return
#' @export
#'
#' @examples
#' data('economics_long')

#' economics_long %>%
#' filter(year(date) == 2000) %>%
#' ggplot(aes(date, value)) +
#' geom_line() +
#' rg_geom_point(aes(fill = variable)) +
#' facet_wrap(~variable, scales = 'free') +
#' theme_bw()

rg_geom_point <- function(mapping = NULL, pch = 21, color = 'white',
                          size = 2, stroke = 1.5, ...) {

  ggplot2::geom_point(mapping = mapping, pch = pch, color = color,
                      size = size, stroke = stroke, ...)
}
