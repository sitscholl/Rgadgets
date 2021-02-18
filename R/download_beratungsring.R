#' Download meteorological data from the beratungsring stations in South Tyrol
#'
#' Functions for accessing the Meteorological Data from the beratungsring stations. The function
#' \code{rg_br_get} can be used to download station measurements for the requested time
#' period and stations. Information about valid station_codes and
#' sensor names can be retrieved using the dataset \link[Rgadgets]{br_info}.
#'
#' @param station_code integer; Id(s) of the stations.See \link[Rgadgets]{br_info} for all available stations and their ids
#' @param user string, access credential
#' @param password string, access password
#' @param host string, host ip adress
#' @param sensor_code string; Name of the sensor(s) of interest (Temperatur 2m for 2m air temperature). See \link[Rgadgets]{br_info} for all available sensors and their names. Use 'simple' to download the most common sensors or NULL to download all sensors
#' @param datestart date; Starting date for the download
#' @param dateend date; End date for the download
#' @param progress display progress bar?
#'
#' @source \url{https://github.com/GiulioGenova/SBR}
#'
#' @format The downloaded data contains the following information:
#' \describe{
#'   \item{st_id}{ID of the station}
#'   \item{datetime}{date and time of measurement}
#'   \item{tair}{Air temperature in 2m height in deg C}
#'   \item{preci}{Precipitation in mm/interval}
#'   \item{relative_air_humidity}{Relative air humidity in percent}
#'   \item{soil_t_25cm}{Soil temperature in 25cm depth in deg C}
#'   \item{tair_dry_60cm}{Dry air temperature at 60cm height in deg C}
#'   \item{tair_wet_60cm}{Wet air temperature at 60cm height in deg C}
#'   \item{supply_voltage}{Battery energy in Volt}
#'   \item{wind_speed}{Wind Speed in m/s}
#'   \item{wind_direction}{Wind direction}
#'   \item{leaf_wetness}{Leaf wetness in percent}
#'   \item{irrigation}{Irrigation on or off}
#'   \item{evaporation}{Evaporation (without unit)}
#'   \item{Drying}{Drying (of the leaves?)}
#'   \item{rad}{Global solar radiation (no unit given)}
#'   \item{tmin}{Minimum air temperature at 2m height during the measurement interval in deg C}
#'   \item{tmax}{Maximum air temperature at 2m height during the measurement interval in deg C}
#'   \item{wetness_min}{Mimimum wetness during the measurement interval in percent}
#'   \item{wetness_max}{Maximum wetness during the measurement interval in percent}
#'   \item{wind_speed_max}{Maximum wind speed during measurement interval in m/s}
#'   \item{irrigation_volumn}{Amount of water from irrigation in  mm}
#'   \item{soil_moisture_20cm}{Soil moisture at 20cm depth in percent}
#'   \item{soil_moisture_40cm}{Soil moisture at 40cm depth in percent}
#'   \item{water_potential_20cm}{Water potential of soil in 20cm depth in kPa}
#'   \item{water_potential_40cm}{Water potential of soil in 40cm depth in kPa}
#'   \item{soil_t_10cm}{Soil temperature at 10cm depth in deg C}
#'   \item{soil_t_30cm}{Soil temperature at 30cm depth in deg C}
#'   \item{t_switch_cabinet}{Temperature of the switch cabinet in deg C}
#'   \item{energy_consumption}{Consumption of energy in Wh}
#'   \item{air_pressure}{Air pressure in hPa}
#'   \item{preci_max_intensity}{Maximum precipitation intensity in mm/h}
#'   \item{preci_type}{Precipitation type}
#'   \item{battery_status}{Remaining energy in battery}
#' }
#'
#' @importFrom magrittr "%>%"
#' @export
#'
#' @examples
#'
#' ##Not Run:
#' station_ids <- Rgadgets::br_info$st_id %>% unique()
#' data <- rg_br_get(station_code = station_ids, host = keyring::key_get('br_host'), password = keyring::key_get('br_password'), user = keyring::key_get('br_user'), sensor_code = NULL)
#'

rg_br_get <- function(station_code,
                      user,
                      password,
                      host,
                      sensor_code = 'simple',
                      datestart = Sys.Date() - 1,
                      dateend = Sys.Date(),
                      progress = F,
                      ...){

  #function to connect to database and request data
  query_SBR_data <- function(year, sensor_query){

    #Connect to database
    con <- DBI::dbConnect(RMariaDB::MariaDB(),
                          dbname = sprintf('sbr_wetter_%s', year),
                          host = host,
                          user =  user,
                          password = password)

    if(year==2013){
      com1="#"
      com2="#"
    }else {
      com1=""
      com2=""
    }

    #Build query string
    query <- sprintf("SELECT \
                 Datum           AS 'TimeStamp', \
                 StationsNr      AS 'id' , \
                 %s \
                 FROM \
                 tab_messung \
                 WHERE \
                 (StationsNr =%s) AND Datum >= '%s' AND Datum <= '%s'",
                     sensor_query,
                     as.character(paste(station_code, collapse = " OR StationsNr =")),
                     datestart,
                     dateend)

    #download data
    db <- DBI::dbGetQuery(con, query)

    #close connection
    DBI::dbDisconnect(con)

    if(year==2013){
      db$BT20=NA
      db$BT40=NA
    }

    return(db %>% tibble::as_tibble())
  }

  stopifnot(all(sapply(list(datestart, dateend), lubridate::is.Date)))

  #add one day to download full data from last day
  dateend = dateend + 1

  #extract information about the sensors
  if(is.null(sensor_code)) sensor_code <- Rgadgets::br_info$measurement %>% unique()
  if(all(sensor_code == 'simple')) sensor_code <- c('Temperatur 2m', 'Niederschlag', 'Bodentemperatur -25cm', 'Relative Luftfeuchtigkeit')

  sensor_table <-
    Rgadgets::br_info %>%
    dplyr::filter(st_id %in% station_code) %>%
    dplyr::filter(measurement %in% sensor_code) %>%

    dplyr::select(column, factor, measurement) %>%
    dplyr::distinct(.keep_all = T) %>%

    dplyr::mutate(measurement = stringr::str_replace_all(measurement, ' ', '_'))

  #build query string for the requested sensors
  sensor_query <- as.character(
    paste0(sensor_table$column,
           "/",
           sensor_table$factor,
           " AS ",
           "'",
           sensor_table$measurement,
           "'",
           collapse=", ")
  )

  #extract years as string
  years <- as.character(seq(as.numeric(year(datestart)),
                            as.numeric(year(dateend), 1)))

  #construct data frame with download parameters
  params_df <-
    expand.grid(years, sensor_query, stringsAsFactors = F) %>%
    tibble::as_tibble() %>%
    rlang::set_names('year', 'sensor_query')

  #progress bar
  if(progress) {
    pb <- progress::progress_bar$new(format = " downloading year :what [:bar] :percent in :elapsed",
                                     total = nrow(params_df), clear = FALSE, width= 80)
    pb$tick(0)
  }

  #iterate over every row and download data
  data_raw <- purrr::pmap_df(params_df, function(...) {

    params <- list(...)

    #tryCatch({query_SBR_data(year = x, sensor_query = y)}, error = function(e){list(NULL)})
    data_download <- query_SBR_data(year = params$year,
                                    sensor_query = params$sensor_query)

    if(progress) pb$tick(tokens = list(what = paste(params$year, '    ')))

    return(data_download)

  })

  #reformat data frame
  if (nrow(data_raw ) > 0) {

    data_prep <-
      data_raw %>%
      dplyr::mutate(datetime = as_datetime(TimeStamp, tz = "Europe/Berlin")) %>%

      dplyr::rename(st_id = id) %>%
      dplyr::rename_with(tolower) %>%

      dplyr::rename_with(
        ~ case_when(
          . == "temperatur_2m" ~ "tair",
          . == "niederschlag" ~ "preci",
          . == 'relative_luftfeuchtigkeit' ~ 'relative_air_humidity',
          . == 'bodentemperatur_-25cm' ~ 'soil_t_25cm',
          . == 'trockentemperatur_60cm' ~ 'tair_dry_60cm',
          . == 'feuchttemperatur_60cm' ~ 'tair_wet_60cm',
          . == 'versorgungsspannung' ~ 'supply_voltage',
          . == 'windgeschwindigkeit' ~ 'wind_speed',
          . == 'windrichtung' ~ 'wind_direction',
          . == 'blattnaesse' ~ 'leaf_wetness',
          . == 'beregnung' ~ 'irrigation',
          . == 'verdunstung' ~ 'evaporation',
          . == 'abtrocknung' ~ 'drying',
          . == 'globale_sonnenstrahlung' ~ 'rad',
          . == 't2m_min_in_messinterval' ~ 'tmin',
          . == 't2m_max_in_messinterval' ~ 'tmax',
          . == 'feuchte_min_in_messinterval' ~ 'wetness_min',
          . == 'feuchte_max_in_messinterval' ~ 'wetness_max',
          . == 'windgeschw._max._in_messinterval' ~ 'wind_speed_max',
          . == 'beregnungsmenge' ~ 'irrigation_volume',
          . == 'bodenfeuchtigkeit_20cm' ~ 'soil_moisture_20cm',
          . == 'bodenfeuchtigkeit_40cm' ~ 'soil_moisture_40cm',
          . == 'bodenwasserpotenzial_20cm' ~ 'water_potential_20cm',
          . == 'bodenwasserpotenzial_40cm' ~ 'water_potential_40cm',
          . == 'bodentemperatur_-10cm' ~ 'soil_t_10cm',
          . == 'bodentemperatur_-30cm' ~ 'soil_t_30cm',
          . == 'schaltschranktemperatur' ~ 't_switch_cabinet',
          . == 'stromverbrauch' ~ 'energy_consumption',
          . == 'luftdruck' ~ 'air_pressure',
          . == 'niederschlagsintensitaet_max' ~ 'preci_max_intensity',
          . == 'niederschlagsart' ~ 'preci_type',
          . == 'ladezustand_batterie' ~ 'battery_status',
          TRUE ~ .
        )
      ) %>%

      dplyr::select(-c(timestamp)) %>%
      dplyr::select(st_id, datetime, everything()) %>%
      dplyr::arrange(st_id, datetime) %>%

      dplyr::filter(datetime < dateend) %>%

      #Sometimes duplicated records appear in table. Keep first row
      dplyr::distinct(st_id, datetime, .keep_all = T)

  } else {stop("No data found. Please modify the Input parameters.")}

  return(data_prep)

}
