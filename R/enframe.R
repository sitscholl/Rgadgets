#' Enframe a list of file paths into a tibble
#'
#' @param folder string, path to folder where the files are stored
#' @param pattern string, a pattern used to identify the files, see \code{\link[base]{list.files}}
#' @param recursive logical, should the listing recurse into directories?
#' @param sep string, separator that is used to split up the basename of the files into different columns.
#' If character, sep is interpreted as a regular expression. The default value is a regular expression that matches any sequence of non-alphanumeric values.
#' If numeric, sep is interpreted as character positions to split at. Positive values start at 1 at the far-left of the string; negative value start at -1 at the far-right of the string. The length of sep should be one less than sep_into.
#' see \code{\link[tidyr]{separate}}
#' @param sep_into string, names of new variables to create as character vector. Use NA to omit the variable in the output.
#'
#' @return
#' @export
#'
#' @examples
#' folder <- 'data/files'
#' df <- rg_enframe(folder, pattern = '.csv$', sep = '_', sep_into = c(NA, 'year', 'type'))
rg_enframe <- function(folder, pattern = '.tif$', recursive = F, sep = '_', sep_into = NULL) {

  files <- list.files(folder, pattern = pattern, full.names = T, recursive = recursive)

  if (is.null(sep_into)) {
    l <- map_dbl(basename(files), ~length(str_split(., pattern = sep)[[1]]))
    sep_into <- str_c('c', 1:max(l))
  }

  result <-
    files %>%
    enframe(NULL, 'path') %>%

    mutate(filename = str_replace(basename(path), pattern, '')) %>%
    separate(filename, into = sep_into, sep = sep)

  if (length(setdiff(c('year', 'month', 'day'), colnames(result))) == 0) {
    result <- mutate(result, date = lubridate::ymd(str_c(year, month, day)))
  }

  return(result)
}
