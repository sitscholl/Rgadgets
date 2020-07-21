#' Bioclimatic index calculation
#'
#' This function allows you to calculate bioclimatic indices
#' from meteorological timeseries.
#' @param dt data.frame or data.table with a column of class 'Date' and the
#' columns mentioned in frml
#' @param index.calc string, Name of the index that you want to calculate.
#' See function get_index_params for further details.
#' @param frml A formula object that describes how to calculate the daily index values. The left side of the formula will be used to name the resulting column and the right side to calculate the daily index values.
#' @param agg.func A function that will be used to aggregate the daily values calculated by 'frml' for every year
#' @param period A sequence of numbers that indicates the day of years that should be used to aggregate the index
#' @param set.zero Are values below 0 possible or should they be converted to 0?
#' @param na.action string, what should happen with NAs in the data? If keep, NAs in the
#' data will translate to NAs in the output, if ignore the index is calculated by leaving
#' out the days with NAs and if fill the NAs can be filled up to maxgap by linear interpolation.
#' @param maxgap integer, up to how many consecutive missing values should be filled?
#' @keywords bioclimatic index
#' @export
#' @import data.table
#' @examples
#' rg_bioclim(dt = dt.agg,
#'   frml = winkler ~ T_mean - 10,
#'   agg.func = sum,
#'   period = 91:304,
#'   set.zero = T)

rg_bioclim <- function(dt, index.calc = 'winkler',
                       frml = NULL, agg.func = NULL,
                       period = NULL, set.zero = NULL,
                       na.action = 'keep', max.gap = 3,
                       ...) {

  if (!inherits(dt, 'data.table')) { dt <- data.table::setDT(dt) }

  #If index name is supplied get parameters from default list
  if (!missing(index.calc)) {
    params <- rg_index_params(index.calc)
    if (is.null(frml)) frml <- params[['frml']]
    if (is.null(agg.func)) agg.func <- params[['agg.func']]
    if (is.null(period)) period <- params[['period']]
    if (is.null(set.zero)) set.zero <- params[['set.zero']]
  }

  #Get index name and formula string from frml
  index.name <- deparse(frml[[2]])
  frml.string <- deparse(frml[[3]])

  #Check if all elements of frml are in dt
  frml.elements <- all.vars(frml[[3]])
  if (!all(frml.elements %in% colnames(dt))) {

    empty.dt <- data.table::data.table('year' = NA,
                                       'index' = NA)

    data.table::setnames(empty.dt, old = c('index'), new = c(index.name))

    warning('Not all frml elements found in column names of dataframe. NAs are returned')
    return(empty.dt)

  }

  #Check if column of class Date is present and extract its name
  col.classes <- sapply(dt, class)
  datecol <- which(sapply(col.classes, function(x) is.element('Date', x)))

  if (length(datecol) == 0) stop('No column of class Date found in dataframe.')

  datecol.name <- colnames(dt)[datecol]

  data.table::setnames(dt, old = c(datecol.name), new = c('Datetime'))

  #Fill in missing days with rows with NAs by join to a complete datesequence
  year.min <- min(as.numeric(format(dt[['Datetime']], '%Y')))
  year.max <- max(as.numeric(format(dt[['Datetime']], '%Y')))

  complete.dates <- data.table::setDT(list('Datetime' = seq.Date(as.Date(str_c(year.min, '-01-01')),
                                                                 as.Date(str_c(year.max, '-12-31')),
                                                                 by='day')))
  data.table::setkey(complete.dates, Datetime)
  data.table::setkey(dt, Datetime)

  #Perform the join to the complete datesequence
  complete.dt <- dt[complete.dates]

  #Extract only the specified days
  complete.dt[,
              ':='('year' = as.numeric(format(Datetime, '%Y')),
                   'doy' = as.numeric(format(Datetime, '%j')))
  ][doy == 366, doy := 365]#Transform doy values of 366 to 365

  complete.dt <- subset(complete.dt, doy %in% period)

  #Optionally fill NAs up to maxgap
  if (na.action == 'fill') {
    complete.dt[,(frml.elements) := lapply(.SD, zoo::na.approx, maxgap = max.gap, na.rm = F), .SDcols = frml.elements]
  }

  #Calculate the daily index value
  complete.dt[,':='('index' = eval(parse(text = frml.string)))]

  #Optionally set values below 0 to 0
  if (set.zero) {
    complete.dt[index < 0, index := 0]
  }

  na.bool <- FALSE
  if (na.action == 'ignore') na.bool <- TRUE

  #Aggregate to yearly values with the given agg.func
  index.dt <- complete.dt[,.('index' = agg.func(index, na.rm = na.bool)), by = 'year']

  data.table::setnames(index.dt, old = c('index'), new = c(index.name))

  return(index.dt)
}
