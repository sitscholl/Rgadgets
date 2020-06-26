#' Classify a datetime sequence into seasons
#'
#' This function classifies single entries of a datetime sequence into the 4 seasons.
#' @param timestamps date or POSIXct sequence
#' @param convention string, a convention that is used to determine the seasons. Can be
#' set to 'northern hemisphere', 'southern hemisphere' or 'month_initials'. In the last
#' case the initials of the months that fall into a single season will be used to name
#' the seasons.
#' @keywords seasons, classification
#' @export
#' @examples
#' classify_season(df$Datetime, convention = 'month_initials')


rg_classify_season <- function(timestamps, convention = 'month_initials') {

  s_terms <- switch(convention,
                    "northern_hemisphere" = c("spring", "summer", "autumn", "winter"),
                    "southern_hemisphere" = c("autumn", "winter", "spring", "summer"),
                    "month_initials"      = c("MAM",    "JJA",    "SON",    "DJF"),
                    stop("Wrong value of convention")
  )

  m <- lubridate::month(timestamps)
  s <- sapply(m,
              function(x) switch(x,
                                 s_terms[4], s_terms[4],
                                 s_terms[1], s_terms[1], s_terms[1],
                                 s_terms[2], s_terms[2], s_terms[2],
                                 s_terms[3], s_terms[3], s_terms[3],
                                 s_terms[4]
              )
  )
  factor(s, levels = s_terms)
}
