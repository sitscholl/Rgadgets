#' Download meteorological data from the province API of South Tyrol
#'
#' Functions for accessing the Meteorological Data from the province of South Tyrol. The function
#' \code{rg_province_get} can be used to download station measurements for the requested time
#' period and stations. Information about valid station_codes and
#' sensors can be retrieved using the function \code{rg_province_info}.
#'
#' @param station_code string; Code of the station
#' @param sensor_code string; Abbreviation of the sensor of interest (e.g. "N" for Precipitation or "LT" for air temperature). Use \code{rg_province_info} for valid sensor names.
#' @param datestart date; Starting date for the download
#' @param dateend date; End date for the download
#' @param format string, wide or long for a wide or long table as result for function \code{rg_province_get} and
#' "table" or "spatial" if the output should be a table or sf-object for function \code{rg_province_info}
#' @param dburl URL; URL of the Province Database
#'
#' @source \url{https://github.com/mattia6690/MonalisR/blob/master/R/Processing.R}
#'
#' @format The downloaded data contains the following information:
#' \describe{
#'   \item{st_id}{ID of the station}
#'   \item{datetime}{date and time of measurement}
#'   \item{tair}{Air temperature in 2m height in deg C}
#'   \item{preci}{Precipitation in mm}
#'   \item{relative_air_humidity}{Relative air humidity in percent}
#'   \item{wind_speed}{Wind Speed in m/s}
#'   \item{wind_direction}{Wind direction in degree}
#'   \item{snow_height}{Snow height in cm}
#'   \item{rad_h}{Duration of sun radiation (Sonnenscheindauer) in s}
#'   \item{rad}{Global solar radiation in W/m2}
#'   \item{water_temperature}{Water temperature in deg C}
#'   \item{water_height}{Height of the water (Wasserstand) in cm}
#'   \item{flow}{Amount of water flowing (Durchfluss) in m3/s}
#'   \item{air_particles}{Concentration of air particles (Schwebstoffkonzentration) in mg/l}
#'   \item{groundwater_height}{Height of the groundwater (Grundwasserstand) in m}
#' }
#'
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
#' @examples
#' Get a vector of available station ids with air temperature measurements
#' ids <- rg_province_info() %>% filter(TYPE == 'LT') %>% pull(SCODE)
#'
#' download data for all stations
#' data <- rg_province_get(station_code = ids, sensor_code = 'LT', datestart = as.Date('20180101', format = '%Y%m%d'), dateend = as.Date('20180105', format = '%Y%m%d'))

rg_province_get <- function(station_code,
                            sensor_code,
                            datestart = Sys.Date() - 1,
                            dateend = Sys.Date(),
                            format = 'wide',
                            dburl = "http://daten.buergernetz.bz.it/services/meteo/v1/timeseries",
                            progress = F,
                            ...){

  stopifnot(all(sapply(list(datestart, dateend), lubridate::is.Date)))

  #If dateseq is more than one year sometimes data at the beginning is missing (because requested data is
  #too large in memory?). Therefore the data is downloaded separately for each year.
  #If the end date is 2019-12-31, for example, the server gives only the first value from this day at 00:00 am
  #and then the download stops. Therefore one day is added to the end day to download all the values from the
  #last day.
  dateseq <- seq.Date(datestart, dateend, by = 'day')
  date_df <-
    dateseq %>%
    split(lubridate::year(.)) %>%
    tibble::enframe(name = 'year', value = 'date') %>%
    tidyr::unnest(date) %>%

    dplyr::group_by(year) %>%
    dplyr::summarise(Start = min(date),
                     End = max(date), .groups = 'drop') %>%
    dplyr::select(-year) %>%

    dplyr::mutate(End = End + 1) %>%
    dplyr::mutate(dplyr::across(tidyselect::everything(), ~format(.x, format = "%Y%m%d")))

  param_df <-
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

    }))

  if(progress) {
    pb <- progress::progress_bar$new(format = " downloading station :what [:bar] :percent in :elapsed",
                                     total = nrow(param_df), clear = FALSE, width= 80)
  }

  dat <-
    param_df %>%
    dplyr::mutate(Data = purrr::pmap(., function(URL, SCODE, ...){
      dat_new <- tryCatch({as_tibble(jsonlite::fromJSON(URL))},error=function(e){NA})

      if(progress) pb$tick(tokens = list(what = paste(SCODE, '    ')))

      return(dat_new)
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
                                           'SD' = 'rad_h',
                                           'WG' = 'wind_speed',
                                           'WR' = 'wind_dir',
                                           'WG.BOE' = 'wind_speed_boe',
                                           'HS' = 'snow_height',
                                           'LF' = 'relative_air_humidity',
                                           'LD.RED' = 'air_pressure',
                                           'WT' = 'water_temperature',
                                           'W' = 'water_height',
                                           'Q' = 'flow',
                                           'SSTF' = 'air_particles',
                                           'W.ABST' = 'groundwater_height')) %>%

      #Sometimes duplicated records appear in table (often on 2018-10-28). Keep first row
      dplyr::distinct(st_id, sensor, datetime, .keep_all = T)

  } else {stop("None of the URLS is valid. Please modify the Input parameters")}

  if (format == 'wide') {

    return(tidyr::pivot_wider(fmt2, names_from = sensor, values_from = value))

  } else if (format == 'long') {

    return(fmt2)

  } else { stop('Unrecognized format string. Use one of "wide" or "long".') }

}

#' @describeIn rg_province_get This function returns all the metainformation of the meteorological Data from the OpenData Portal South Tyrol.
#' @export

rg_province_info <- function(format="table") {

  stat      <- jsonlite::fromJSON("http://daten.buergernetz.bz.it/services/meteo/v1/stations")
  stat.prop <- stat$features$properties
  sens.prop <- jsonlite::fromJSON("http://daten.buergernetz.bz.it/services/meteo/v1/sensors")

  ret <- merge(stat.prop, sens.prop, by="SCODE")

  if (format == "table")   ret <- ret %>% tibble::as_tibble()
  if (format == "spatial") ret <- sf::st_as_sf(ret,coords=c("LONG","LAT"), crs=4326, na.fail = F)

  return(ret)
}
