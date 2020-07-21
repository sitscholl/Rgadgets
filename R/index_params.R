#' Default index parameters for different bioclimatic indices
#'
#' This function stores some default properties of bioclimatic indices.
#' @param x character, name of the bioclimatic index. Possible values are: 'huglin', 'winkler',
#' 'GSTavg', 'cni', or 'rad_sum'. There are some predefined indices within this function
#' (Currently: Winkler, Huglin, GSTavg, rad_sum). To use this parameter,
#' the columns in the input df have to be
#' named the following way: tmax, tmin, tmean for maximum, minimum and mean
#' temperature and rad for solar radiation.
#' @keywords bioclimatic index, parameters
#' @export
#' @import data.table
#' @examples
#' get_index_params('huglin')

rg_index_params <- function(x) {

  index.param.list <- list('huglin' = list('frml' = huglin ~ (((((tmax + tmin) / 2) - 10) + (tmax - 10)) / 2) * 1.05,
                                           'agg.func' = sum,
                                           'period' = 91:273,
                                           'set.zero' = T),
                           'winkler' = list('frml' = winkler ~ ((tmax + tmin) / 2) - 10,
                                            'agg.func' = sum,
                                            'period' = 91:304,
                                            'set.zero' = T),
                           'GSTavg' = list('frml' = GSTavg ~ tmean,
                                           'agg.func' = mean,
                                           'period' = 91:304,
                                           'set.zero' = F),
                           'cni' = list('frml' = cni ~ tmin,
                                        'agg.func' = mean,
                                        'period' = 245:274,
                                        'set.zero' = F),
                           'rad_sum' = list('frml' = rad_sum ~ rad,
                                            'agg.func' = sum,
                                            'period' = 91:304,
                                            'set.zero' = F))

  if (!x %in% names(index.param.list)) {
    stop('Index name has not been found in parameter list')
  }

  return(index.param.list[[x]])
}
