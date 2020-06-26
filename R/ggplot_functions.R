#' Adding model equation to ggplot. Wrapper around ggpmisc::stat_poly_eq() with standard options for label
#'
#' @param formula Model formula
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

rg_geom_equation <- function(formula = y~x, ...) {

  ggpmisc::stat_poly_eq(parse = T,
                        formula = formula,
                        aes(label = paste(..eq.label..,
                                          ..rr.label..,
                                          p.value.label,
                                          sep = "*plain(\";\")~~")),
                        ...)

}
