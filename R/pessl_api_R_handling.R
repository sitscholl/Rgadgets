#' Transform response from pessl-api into a dataframe
#'
#' This function allows you to transform the list-object that is returned from the pessl-api
#' into a data.frame
#' @param api_response list-object that is returned from the get_data function in the python script "pessl_api"
#' @param sensor_list a list of sensor = new_name pairs. Determines which sensors are used from the stations
#' and how they are named in the resulting data.frame
#' @param time_res string, resolution of the downloaded data. possible values are raw, hourly, daily or monthly
#' @keywords pessl, api, meteo stations
#' @export
#' @examples
#' df <- response_to_df(response, sensor_list = list('Air temperature, high precision' = 'T_air'), time_res = 'raw)

rg_response_to_df <- function(api_response, sensor_list = NULL, time_res = 'daily') {

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
  values_sub_simple <- purrr::map(values_sub_NA, ~purrr::map_if(., .p = is.list, .f = flatten_dbl))

  #Sometimes two lists for one sensor. If TRUE they are combined into a single one
  list_names <- names(values_sub_simple)
  if (any(duplicated(list_names))) {

    # clean_list <- values_sub_simple[which(!duplicated(names(values_sub_simple), fromLast = T))]

    dup_names <- list_names[which(duplicated(list_names))]
    clean_list <- values_sub_simple[which(!list_names %in% dup_names)]

    for (dup_name in dup_names) {

      list_elements <- values_sub_simple[which(names(values_sub_simple) %in% dup_name)]
      dup_df <- bind_cols(list_elements)

      names_df <- str_replace(colnames(dup_df), '[0-9]', '')
      dup_cols <- names_df[which(duplicated(names_df))]

      sub_list <- vector(mode = 'list', length = length(dup_cols))
      sub_list <- set_names(sub_list, dup_cols)
      for (j in seq_along(dup_cols)) {
        name <- dup_cols[[j]]

        fill <-
          dup_df %>%

          dplyr::select_if(.predicate = str_detect(colnames(.), name)) %>%
          mutate(combined = pmap_dbl(., function(...) {

            vec <- c(...)
            if (all(is.na(vec))) return(NA)

            tail(na.omit(vec), 1)

          })) %>%

          pull(combined)

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
      purrr::set_names(str_c(parameter, names(.), sep = '_')) %>%
      dplyr::mutate(date = dates)

    station_ts <- dplyr::left_join(station_ts, df_vals, by = 'date')

  }

  return(station_ts)

}
