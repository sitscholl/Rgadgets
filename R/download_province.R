#' Download meteorological data from the province API of South Tyrol
#'
#' Function for accessing the Meteorological Data from the province of South Tyrol.
#' @param dburl URL; URL of the Province Database. If left empty the original API will be used
#' @param station_code string; Code of the station ("SCODE")
#' @param sensor_code string; Abbreviation of the sensor of interest (e.g. "N" for Precipitation).
#' For possible values check \code{\link{rg_province_info}}.
#' @param datestart date; Starting date for the download
#' @param dateend date; End date for the download
#' @param format string, wide or long for a wide or long table as ar result
#' @source \url{https://github.com/mattia6690/MonalisR/blob/master/R/Processing.R}
#' @importFrom dplyr mutate select rename arrange
#' @importFrom magrittr "%>%" extract
#' @importFrom lubridate as_datetime
#' @importFrom stats setNames
#' @importFrom tidyr unnest
#' @importFrom jsonlite fromJSON
#' @importFrom tibble as_tibble
#' @importFrom purrr pmap_chr map_df
#' @importFrom tidyselect everything
#' @export

rg_province_get <- function(dburl=NULL, station_code, sensor_code, datestart, dateend, format = 'wide'){

  #use url from province database if non given
  if(is.null(dburl)) dburl<- "http://daten.buergernetz.bz.it/services/meteo/v1/timeseries"

  #If dateseq is more than one year sometimes data at the beginning is missing (because requested data is
  #too large in memory?). Therefore the data is downloaded separately for each year.
  #If the end date is 2019-12-31, for example, the server gives only the first value from this day at 00:00 am
  #and then the download stops. Therefore one day is added to the end day to download all the values from the
  #last day.
  dateseq <- seq.Date(datestart, dateend, by = 'day')
  date_df <-
    dateseq %>%
    split(year(.)) %>%
    tibble::enframe(name = 'year', value = 'date') %>%
    tidyr::unnest(date) %>%

    dplyr::group_by(year) %>%
    dplyr::summarise(Start = min(date),
                     End = max(date), .groups = 'drop') %>%
    dplyr::select(-year) %>%

    mutate(End = End + 1) %>%
    dplyr::mutate(dplyr::across(tidyselect::everything(), ~format(.x, format = "%Y%m%d")))

  dat <-
    expand.grid(station_code, sensor_code, stringsAsFactors = FALSE) %>%
    tibble::as_tibble() %>%
    setNames(c("SCODE","Sensor")) %>%

    dplyr::mutate(dates = list(date_df)) %>%
    tidyr::unnest(dates) %>%

    dplyr::mutate(URL = pmap_chr(., function(SCODE, Sensor, Start, End){

      dburl %>%
        paste0(.,"?station_code=",SCODE) %>%
        paste0(.,"&output_format=JSON") %>%
        paste0(.,"&sensor_code=",Sensor) %>%
        paste0(.,"&date_from=",Start) %>%
        paste0(.,"&date_to=",End)

    })) %>%

    dplyr::mutate(Data= lapply(URL, function(x){
      tryCatch({as_tibble(jsonlite::fromJSON(x))},error=function(e){NA})
    }))

  fmt <-
    dat %>%
    unnest(cols = c(Data))

  if (nrow(fmt) > 0) {

    fmt2 <-
      fmt %>%
      dplyr::mutate(datetime = as_datetime(DATE, tz = "Europe/Berlin")) %>%

      dplyr::rename(st_id = SCODE) %>%
      dplyr::rename_with(tolower) %>%

      dplyr::select(-c(url, date, start, end)) %>%
      dplyr::select(st_id, sensor, datetime, everything()) %>%
      dplyr::arrange(st_id, sensor, datetime) %>%

      dplyr::mutate(sensor = dplyr::recode(sensor,
                                           'LT' = 'tair',
                                           'GS' = 'rad',
                                           'N' = 'preci',
                                           'SD' = 'radh')) %>%

      #Sometimes duplicated records appear in table (often on 2018-10-28). Keep first row
      dplyr::distinct(st_id, sensor, datetime, .keep_all = T)

  } else {stop("None of the URLS is valid. Please modify the Input parameters")}

  if (format == 'wide') {

    return(tidyr::pivot_wider(fmt2, names_from = sensor, values_from = value))

  } else if (format == 'long') {

    return(fmt2)

  } else { stop('Unrecognized format string. Use one of "wide" or "long".') }

}

#' Return Meteo South Tyrol Metainformation
#'
#' This function returns all the metainformation of the meteorological Data from
#' the OpenData Portal South Tyrol, It is a combination of both former functions `getMeteoStat` and `getMeteoSensor`.
#' It unifies both information returning the complete range of information present in the Open Data Portal South Tyrol.
#' @param format string; digit "table" if the output should be a Dataframe or "spatial" for a spatial
#' output as sf-object
#' @source \url{https://github.com/mattia6690/MonalisR/blob/master/R/Processing.R}
#' @importFrom jsonlite fromJSON
#' @importFrom sf st_as_sf
#' @export

rg_province_info <- function( format="table" ){

  stat      <- jsonlite::fromJSON("http://daten.buergernetz.bz.it/services/meteo/v1/stations")
  stat.prop <- stat$features$properties
  sens.prop <- jsonlite::fromJSON("http://daten.buergernetz.bz.it/services/meteo/v1/sensors")

  ret <- merge(stat.prop,sens.prop,by="SCODE")

  if (format == "table")   ret<-ret
  if (format == "spatial") ret<-st_as_sf(ret,coords=c("LONG","LAT"),crs=4326,na.fail = F)

  return(ret)
}
