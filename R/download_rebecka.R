#' Download REBECKA station data
#'
#' These functions download station data from the REBECKA stations using an API to the pessl database.
#' The function \code{rg_rebecka_ids} can be used to request the available station ids which are in turn
#' used in the function \code{rg_rebecka_get} to download data for a desired time period. Data from the
#' stations is available from 2017 - 2020 in 10 minute (raw), hourly and daily resolution. Downloading
#' requires a public and private Key. The functions internally use the package \code{reticulate} to
#' source a python script and connect to the API. The python script was written in python 2.7, it is
#' not guaranteed to work with newer python versions.
#'
#' @param station_code character, IDs of the station(s) to download
#' @param datestart date, starting date for download
#' @param dateend date, ending date for download
#' @param url string, url for the API
#' @param publicKey string, public KEY used for API authentication
#' @param privateKey string, private KEY used for API authentication
#' @param time_res string, time resolution of the downloaded data, one of 'raw', 'hourly' or 'daily'.
#'
#' @return
#' @export
#' @source \url{https://api.fieldclimate.com/v1/docs/}
#'
#' @import reticulate
#'
#' @examples
#' Get a vector of available station ids
#' ids <- rg_rebecka_ids(publicKey = XXX, privateKey = XXX)
#'
#' download data for the first station at daily resolution for one year
#' data <- rg_rebecka_get(station_code = ids[[1]], publicKey = XXX, privateKey = XXX, datestart = as.Date('20180101', format = '%Y%m%d'), dateend = as.Date('20180105', format = '%Y%m%d'), time_res = 'daily')

rg_rebecka_get <- function(station_code,
                           publicKey = '056c0e506344aff17f6b60237d9dabcb87f1e8b4',
                           privateKey,
                           datestart = Sys.Date() - 1,
                           dateend = Sys.Date(),
                           url = 'https://api.fieldclimate.com/v1',
                           time_res = 'raw',
                           progress = T) {

  stopifnot(all(sapply(list(datestart, dateend), lubridate::is.Date)))

  path <- system.file("data/pessl_api.py", package = "Rgadgets")
  reticulate::source_python(path)

  #If dateseq is too long an error occurs because too much data has to be downloaded at once. Therefore
  #data is downloaded separately for each month
  dateseq <- seq.Date(datestart, dateend, by = 'day')
  date_df <-
    dateseq %>%
    split(list(lubridate::year(.),
               lubridate::month(.))) %>%
    tibble::enframe(name = 'year', value = 'date') %>%
    tidyr::unnest(date) %>%

    dplyr::group_by(year) %>%
    dplyr::summarise(start = min(date),
                     end = max(date), .groups = 'drop') %>%
    dplyr::select(-year) %>%

    dplyr::arrange(start) %>%
    dplyr::mutate(end = end + 1)

  #Create table with station id, start and end dates per month
  param_df <-
    station_code %>%
    tibble::enframe(NULL, 'st_id') %>%
    dplyr::mutate(dates = list(date_df)) %>%
    tidyr::unnest(dates)

  if(progress) {
    pb <- progress::progress_bar$new(format = " downloading station :what [:bar] :percent in :elapsed",
                                     total = nrow(param_df), clear = FALSE, width= 80)
    pb$tick(0)
    }

  #Download and reformat data
  data <- purrr::pmap_df(param_df, function(...) {

    params <- list(...)

    data_raw <- py_api_get_data(st_id = params$st_id,
                                datestart = params$start,
                                dateend = params$end,
                                url = url,
                                publicKey = publicKey,
                                privateKey = privateKey,
                                time_res = time_res)

    #if no data is present or error occurs the error code is returned in data_raw
    if(is.integer(data_raw)) {

      # warning(sprintf('No data returned for station %s and period %s - %s with error code %d',
      #                 params$st_id,
      #                 params$start,
      #                 params$end,
      #                 data_raw))

      if(progress) pb$tick(tokens = list(what = paste(params$st_id, '    ')))

      return(tibble::tibble())

      }

    data_re <-
      data_raw %>%
      Rgadgets:::rg_response_to_df(time_res = time_res) %>%
      dplyr::mutate(st_id = params$st_id) %>%
      dplyr::select(st_id, tidyselect::everything())

    if(progress) pb$tick(tokens = list(what = paste(params$st_id, '    ')))

    return(data_re)

  })

  return(data)
}

#' @describeIn rg_rebecka_get Request station ids for the REBECKA stations
#' @export

rg_rebecka_codes <- function(publicKey = '056c0e506344aff17f6b60237d9dabcb87f1e8b4',
                             privateKey,
                             url = 'https://api.fieldclimate.com/v1') {

  path <- system.file("data/pessl_api.py", package = "Rgadgets")
  reticulate::source_python(path)

  ids <- py_api_get_st_ids(url = url,
                           publicKey = publicKey,
                           privateKey = privateKey)

  return(ids)

}

#' Transform response from pessl-api into a dataframe
#'
#' This function allows you to transform the list-object that is returned from the pessl-api
#' into a data.frame
#' @param api_response list-object that is returned from the get_data function in the python script "pessl_api"
#' @param sensor_list a list of sensor = new_name pairs. Determines which sensors are used from the stations
#' and how they are named in the resulting data.frame
#' @param time_res string, resolution of the downloaded data. possible values are raw, hourly, daily or monthly
#' @keywords pessl, api, meteo stations

rg_response_to_df <- function(api_response, sensor_list = NULL, time_res = 'raw') {

  #If not given use default list that describes which sensors from the station should be
  #extracted and the new name in the resulting dataframe
  if (is.null(sensor_list)) {
    sensor_list <- list('Air temperature, high precision' = 'T',
                        'HC Air temperature' = 'T',
                        'Drybulb temperature, high precision' = 'T_90cm',
                        'Air temperature' = 'T_90cm',
                        'Solar radiation' = 'rad',
                        'MPS-2 Water potential' = 'soil_wp',
                        'MPS-2 temperature' = 'soil_t')
  }

  #Define functions how to format the dates from the api response
  date_func <- switch(time_res,
                      "daily" = lubridate::as_date,
                      "monthly" = lubridate::as_date,
                      "raw" = lubridate::as_datetime,
                      "hourly" = lubridate::as_datetime
  )

  #reformat dates
  dates <- date_func(api_response$dates)

  #extract measured values from all available sensors
  values <- api_response$data
  names(values) <- purrr::map_chr(values, 'name')

  #use only sensors from sensor_list
  values_sub <- values[which(names(values) %in% names(sensor_list))]
  names(values_sub) <- purrr::map_chr(names(values_sub), function(x) sensor_list[[x]])

  #Extract the measurements and remove all the other data. Then transform the measurements
  #from lists to vectors
  values_sub_values <- purrr::map(values_sub, 'aggr')
  values_sub_NA <- purrr::map(values_sub_values, ~purrr::map_if(., .p = is.list, .f = ~replace(., sapply(., is.null), NA_integer_)))
  values_sub_simple <- purrr::map(values_sub_NA, ~purrr::map_if(., .p = is.list, .f = purrr::flatten_dbl))

  #Sometimes two lists for one sensor. If TRUE they are combined into a single one
  list_names <- names(values_sub_simple)
  if (any(duplicated(list_names))) {

    # clean_list <- values_sub_simple[which(!duplicated(names(values_sub_simple), fromLast = T))]

    #Extract names that are duplicated
    dup_names <- list_names[which(duplicated(list_names))]

    #remove duplicated from list
    clean_list <- values_sub_simple[which(!list_names %in% dup_names)]

    #Iterate over the duplicated entries and combine them into one
    for (dup_name in dup_names) {

      #Extract duplicated entries
      list_elements <- values_sub_simple[which(names(values_sub_simple) %in% dup_name)]

      #remove level of hierarchy
      list_elements_flat <- do.call("c", list_elements)

      #remove suffix
      prefixes <- stringr::str_c(unique(names(list_elements)), '.')
      names(list_elements_flat) <- stringr::str_replace_all(names(list_elements_flat),
                                                            prefixes,
                                                            rep('', length(prefixes)))

      #generate unique names
      names(list_elements_flat) <- stringr::str_c(names(list_elements_flat), 1:length(list_elements_flat))

      #Combine into dataframe
      dup_df <- dplyr::bind_cols(list_elements_flat)

      names_df <- stringr::str_replace(colnames(dup_df), '[0-9]+$', '')
      dup_cols <- names_df[which(duplicated(names_df))]

      sub_list <- vector(mode = 'list', length = length(dup_cols))
      sub_list <- rlang::set_names(sub_list, dup_cols)
      for (j in seq_along(dup_cols)) {
        name <- dup_cols[[j]]

        fill <-
          dup_df %>%

          dplyr::select_if(.predicate = stringr::str_detect(colnames(.), name)) %>%
          dplyr::mutate(combined = purrr::pmap_dbl(., function(...) {

            vec <- c(...)
            if (all(is.na(vec))) return(NA)

            tail(na.omit(vec), 1)

          })) %>%

          dplyr::pull(combined)

        sub_list[[j]] <- fill
      }
      clean_list[[dup_name]] <- sub_list
    }

  } else {
    clean_list <- values_sub_simple
  }

  #Iterate over all remaining sensors and generate a dataframe with the sensor combined with
  #average, min or max as column name and the measured values in the rows
  station_ts <- tibble::tibble(date = dates)
  for (i in seq_along(clean_list)) {

    parameter <- names(clean_list)[[i]]

    df_vals <-
      dplyr::bind_rows(clean_list[[i]]) %>%
      purrr::set_names(stringr::str_c(parameter, names(.), sep = '_')) %>%
      dplyr::mutate(date = dates)

    station_ts <- dplyr::left_join(station_ts, df_vals, by = 'date')

  }

  return(station_ts)

}
