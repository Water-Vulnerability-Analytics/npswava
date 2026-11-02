#' @title Water year of each date
#'
#' @description
#' Returns the water year each date falls in. A water year starts on the
#' first day of `water_year_start_month` and is labelled by the calendar
#' year it ends in, so with the default (October) water year 2021 runs from
#' October 2020 to September 2021.
#'
#' @param dates Date vector, or anything [lubridate::as_date()] can read
#'   (e.g. `"2020-10-15"`). Required.
#' @param water_year_start_month Integer from 2 to 12. The month the water
#'   year starts in.
#'   * `10`: October to September, the usual US water year.
#'   * Another month, e.g. `11`: dates from that month on belong to the next
#'     calendar year's water year.
#'
#'   With `1`, every date is put one year ahead, so use calendar years
#'   instead. Default: `10`.
#'
#' @return A numeric vector of water years, the same length as `dates`.
#' @export
#' @seealso [make_ym()] to group dates by month.
#' @examples
#' calc_water_year(c("2020-09-30", "2020-10-01", "2021-03-15"))
#'
#' # A water year that starts in November
#' calc_water_year(c("2020-10-15", "2020-11-15"), water_year_start_month = 11)

calc_water_year <- function(dates, water_year_start_month = 10) {
  # Ensure the input is treated as a date (lubridate functions handle various formats)
  dates <- lubridate::as_date(dates)
  
  # Extract calendar year and month
  calendar_year <- lubridate::year(dates)
  month_of_year <- lubridate::month(dates)
  
  # Apply the water year logic:
  # If the month is October (10) or later, the water year is the next calendar year.
  # Otherwise (January to September), the water year is the current calendar year.
  water_year <- ifelse(month_of_year >= water_year_start_month, calendar_year + 1, calendar_year)
  
  return(water_year)
}
