#' Bulk download Meteorological Data (v2)
#'
#' Function for accessing the Meteorological Data. This function will replace the
#' Original Function in the next version. No save to CSV is possible anymore. On the other side
#' it will be possible to speed up the actual processing times and to make multiple requests at once
#' @param dburl URL; URL of the Province Database. If left empty the original API will be used
#' @param station_code string; Code of the station ("SCODE")
#' @param sensor_code string; Abbreviation of the sensor of interest (e.g. "N" for Precipitation)
#' @param datestart string; Starting time for the download in "Ymd" Format
#' @param dateend string; End time for the download in "Ymd" Format
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

rg_get_province <- function(dburl=NULL, station_code, sensor_code, datestart, dateend){

  if(is.null(dburl)) dburl<- "http://daten.buergernetz.bz.it/services/meteo/v1/timeseries"
  datestart1 <- convertDate(datestart, db="Meteo")
  dateend1   <- convertDate(dateend, db="Meteo")

  dat<-expand.grid(station_code,sensor_code,stringsAsFactors = FALSE) %>%
    as_tibble() %>%
    setNames(c("SCODE","Sensor")) %>%
    mutate(Start=format(as.Date(datestart),format="%Y%m%d")) %>%
    mutate(End=format(as.Date(dateend),format="%Y%m%d")) %>%
    mutate(URL=pmap_chr(.,function(SCODE,Sensor,Start,End){

      dburl %>%
        paste0(.,"?station_code=",SCODE) %>%
        paste0(.,"&output_format=JSON") %>%
        paste0(.,"&sensor_code=",Sensor) %>%
        paste0(.,"&date_from=",Start) %>%
        paste0(.,"&date_to=",End)

    })) %>%
    mutate(Data= lapply(URL, function(x){
      tryCatch({as_tibble(jsonlite::fromJSON(x))},error=function(e){NA})
    }))


  fmt<- unnest(dat)
  if(nrow(fmt)>0){

    fmt2<- fmt %>%
      mutate(TimeStamp=as_datetime(DATE,tz = "Europe/Berlin")) %>%
      rename(Value=VALUE) %>%
      select(-c(URL,DATE)) %>%
      select(TimeStamp, everything()) %>%
      arrange(Sensor,TimeStamp)

    return(fmt2)
  } else {stop("None of the URLS is valid. Please modify the Input parameters")}

}

#' Return Meteo South Tyrol Metainformation
#'
#' This function returns all the metainformation of the meteorological Data from
#' the OpenData Portal South Tyrol, It is a combination of both former functions `getMeteoStat` and `getMeteoSensor`.
#' It unifies both information returning the complete range of information present in the Open Data Portal South Tyrol.
#' @param format string; digit "table" if the output should be a Dataframe or "spatial" for a spatial
#' output as sf-object
#' @importFrom jsonlite fromJSON
#' @importFrom sf st_as_sf
#' @export

rg_province_info <- function(format="table"){

  stat      <- jsonlite::fromJSON("http://daten.buergernetz.bz.it/services/meteo/v1/stations")
  stat.prop <- stat$features$properties
  sens.prop <- jsonlite::fromJSON("http://daten.buergernetz.bz.it/services/meteo/v1/sensors")

  ret <- merge(stat.prop,sens.prop,by="SCODE")

  if (format == "table")   ret<-ret
  if (format == "spatial") ret<-st_as_sf(ret,coords=c("LONG","LAT"),crs=4326,na.fail = F)

  return(ret)
}
