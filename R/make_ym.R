#' @title Convert dates to the first day of their month
#'
#' @description
#' Takes the first seven characters of each date (`"YYYY-MM"`) and converts
#' them with [lubridate::ym()], so every date becomes the first day of its
#' month. Useful for grouping daily data by month.
#'
#' @param date Character vector or Date vector. Dates whose first seven
#'   characters are `"YYYY-MM"`, e.g. `"2020-06-15"` or `"2020-06"`. Values
#'   that can't be read give `NA` with a warning. Required.
#'
#' @return A Date vector, each value the first day of its month.
#' @export
#' @seealso [calc_water_year()] to group dates by water year.
#' @examples
#' make_ym(c("2020-06-15", "2020-06-30", "2021-01"))
#' make_ym(as.Date("2023-10-09"))

make_ym <- function(date) {
  lubridate::ym(substr(date, 1, 7))
}
