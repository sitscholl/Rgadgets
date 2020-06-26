#' Adding model equation to ggplot. Wrapper around ggpmisc::stat_poly_eq() with standard options for label
#'
#' @param formula Formula, Model formula
#' @param elements String or vector of strings, The parts of the model statistics that should be included in the output
#' @param sep String, separator between model elements
#' @param ... Further parameters passed on to ggpmisc::stat_poly_eq()
#' one of index or name.
#' @keywords ggplot, model, equation
#' @export
#' @examples
#'
#' my_formula <- y~x
#' ggplot(data, aes(x, y)) +
#' geom_point() +
#' geom_smooth(method = 'lm', formula = my_formula) +
#' geom_equation(formula = my_formula)

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
