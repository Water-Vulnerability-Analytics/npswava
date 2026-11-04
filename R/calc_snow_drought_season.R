#' @title Snow drought status for every day of the cool season
#'
#' @description
#' Classifies snow drought day by day across the cool season, for one or more
#' groups (e.g. SNOTEL stations, or climate futures), following the Hatchett,
#' Rhoades and McEvoy (2022) framework. NIDIS uses 20 percent to define snow
#' drought, as for other drought variables. The function builds a smoothed
#' day-of-year percentile climatology per group from a moving window, then
#' classifies every day in the record against it.
#'
#' It works for observed station data (group by station, so each station's
#' full record is its own climatology) and for modeled projections (group by
#' GCM and use `climatology_filter` to mark the historical run, so future
#' scenarios are scored against historical conditions).
#'
#' @details
#' `drought_type` comes from the SWE and precipitation percentiles:
#' * `"No drought"`: SWE at or above the 30th percentile.
#' * `"Dry"`: SWE and precipitation both below the 30th percentile.
#' * `"Warm-Dry"`: SWE below the 30th, precipitation from the 30th up to the
#'   50th.
#' * `"Warm"`: SWE below the 30th, precipitation at or above the 50th.
#'
#' A percentile is interpolated between the 10th, 20th, 30th, 50th, 70th, 80th
#' and 90th percentile values of the window. A value at or below the 10th
#' percentile value gets 0, and one at or above the 90th gets 100.
#'
#' @param df Data frame with at least date, SWE and precipitation columns.
#'   Required.
#' @param date_col String. The name of the date column. Default: `"date"`.
#' @param swe_col String. The name of the snow water equivalent column.
#'   Default: `"swe_in"`.
#' @param precip_col String. The name of the water-year-to-date cumulative
#'   precipitation column. Default: `"precip_in"`.
#' @param group_col String or `NULL`. A column to build separate climatologies
#'   for.
#'   * A string, e.g. `"name"` for stations or `"GCM"` for projections: one
#'     climatology per value, and the column is kept in the output.
#'   * `NULL`: `df` is treated as a single series.
#'
#'   Default: `NULL`.
#' @param climatology_filter Logical vector the same length as `df`'s rows,
#'   or `NULL`. Which rows build the climatology.
#'   * A logical vector, e.g. `df$CF == "Historical"`: only `TRUE` rows build
#'     the climatology. All rows are still classified against it.
#'   * `NULL`: every row builds the climatology of its group, as is right for
#'     observed station records.
#'
#'   Default: `NULL`.
#' @param cool_season_months Numeric vector. The months to classify; other
#'   months are dropped. Default: `c(11, 12, 1, 2, 3, 4, 5)`, November to May,
#'   when snow drought is meaningful.
#' @param min_years Number. The years of climatology wanted per group. A group
#'   with fewer gets a warning, and is still classified. NIDIS wants 20 or
#'   more. Default: `20`.
#' @param window Number. Days on either side of a day of year to pool when
#'   building the percentile climatology. Default: `3`, a 7-day window.
#'
#' @return A data frame with one row per date and group in the cool season:
#'   `date`, the `group_col` column (if any), `water_year`, `month_day`,
#'   `swe_in`, `precip_in`, `swe_pctile`, `precip_pctile` and `drought_type`
#'   (a factor). Rows with a missing SWE or precipitation value are dropped.
#' @export
#' @seealso [get_snotel()] for station data, and [calc_water_year()] for the
#'   water years in the output.
#' @examples
#' dates <- seq(as.Date("1999-11-01"), as.Date("2021-05-31"), by = "day")
#' dates <- dates[as.numeric(format(dates, "%m")) %in% c(11, 12, 1:5)]
#' year_effect <- 1 + 0.4 * sin(as.numeric(format(dates, "%Y")) * 2.7)
#' snow <- data.frame(
#'   date = dates,
#'   swe_in = 10 * year_effect + as.numeric(format(dates, "%j")) %% 30 / 10,
#'   precip_in = 20 * rev(year_effect) + as.numeric(format(dates, "%j")) %% 20 / 10
#' )
#'
#' droughts <- calc_snow_drought_season(snow)
#' table(droughts$drought_type)

calc_snow_drought_season <- function(df,
                                     date_col = "date",
                                     swe_col = "swe_in",
                                     precip_col = "precip_in",
                                     group_col = NULL,
                                     climatology_filter = NULL,
                                     cool_season_months = c(11, 12, 1, 2, 3, 4, 5),
                                     min_years = 20,
                                     window = 3) {

  df <- df |>
    dplyr::rename(date = dplyr::all_of(date_col),
                  swe_in = dplyr::all_of(swe_col),
                  precip_in = dplyr::all_of(precip_col))

  df <- if (is.null(group_col)) {
    df |> dplyr::mutate(.group = "all")
  } else {
    df |> dplyr::mutate(.group = .data[[group_col]])
  }

  if (is.null(climatology_filter)) climatology_filter <- rep(TRUE, nrow(df))

  df <- df |>
    dplyr::mutate(.in_climatology = climatology_filter,
                 water_year = calc_water_year(date),
                 month_day = format(date, "%m-%d")) |>
    dplyr::filter(lubridate::month(date) %in% cool_season_months,
                 !is.na(swe_in), !is.na(precip_in))

  # Check years of climatology history per group
  year_counts <- df |>
    dplyr::filter(.in_climatology) |>
    dplyr::group_by(.group) |>
    dplyr::summarise(n_years = dplyr::n_distinct(water_year), .groups = "drop")

  thin_groups <- year_counts |> dplyr::filter(n_years < min_years)
  if (nrow(thin_groups) > 0) {
    warning(paste0("Fewer than ", min_years, " years of climatology history for: ",
                   paste(thin_groups$.group, collapse = ", ")))
  }

  # +/- window-day pool of month_days for a given focal day, anchored to a
  # fixed reference year so wraparound (e.g. Dec -> Jan) works
  get_window_mds <- function(focal_md, all_mds) {
    # turns the "MM-DD" string into a real Date object by 
    # anchoring it to an arbitrary placeholder year (2000, chosen 
    # specifically because it's a leap year, so "02-29" doesn't break). 
    focal_date <- as.Date(paste0("2000-", focal_md))
    window_dates <- seq(focal_date - window, focal_date + window, by = "day")
    window_mds <- format(window_dates, "%m-%d")
    window_mds[window_mds %in% all_mds]
  }

  # Percentile breakpoints for one variable, per group, from climatology rows only
  build_climatology <- function(clim_df, variable) {
    all_md <- clim_df |> dplyr::distinct(month_day) |> dplyr::arrange(month_day) |> dplyr::pull(month_day)

    clim_df |>
      dplyr::group_split(.group) |>
      purrr::map(function(grp_df) {
        grp_name <- grp_df$.group[1]
        purrr::map(all_md, function(md) {
          window_mds <- get_window_mds(md, all_md)
          pool <- grp_df |> dplyr::filter(month_day %in% window_mds) |> dplyr::pull({{ variable }})
          pool <- pool[!is.na(pool)]
          if (length(pool) < 5) return(NULL)
          tibble::tibble(
            .group = grp_name,
            month_day = md,
            p10 = stats::quantile(pool, 0.10), p20 = stats::quantile(pool, 0.20),
            p30 = stats::quantile(pool, 0.30), p50 = stats::quantile(pool, 0.50),
            p70 = stats::quantile(pool, 0.70), p80 = stats::quantile(pool, 0.80),
            p90 = stats::quantile(pool, 0.90)
          )
        }) |>
          purrr::list_rbind()
      }) |>
      purrr::list_rbind()
  }

  climatology_rows <- df |> dplyr::filter(.in_climatology)

  clim_swe <- build_climatology(climatology_rows, swe_in)
  clim_precip <- build_climatology(climatology_rows, precip_in)

  interpolate_pctile <- function(x, p10, p20, p30, p50, p70, p80, p90) {
    breaks <- c(p10, p20, p30, p50, p70, p80, p90)
    probs  <- c(0.10, 0.20, 0.30, 0.50, 0.70, 0.80, 0.90)

    if (is.na(x)) return(NA_real_)
    if (x <= breaks[1]) return(0)
    if (x >= breaks[7]) return(100)

    idx <- findInterval(x, breaks)
    lo <- breaks[idx]; hi <- breaks[idx + 1]
    plo <- probs[idx]; phi <- probs[idx + 1]

    round((plo + (x - lo) / (hi - lo) * (phi - plo)) * 100, 1)
  }

  # Climatology is built from climatology_filter rows only, but applied to
  # every row in the group - so future/non-climatology rows still get scored
  result <- df |>
    dplyr::left_join(clim_swe, by = c(".group", "month_day"), suffix = c("", "_swe")) |>
    dplyr::left_join(clim_precip, by = c(".group", "month_day"), suffix = c("", "_prc")) |>
    dplyr::mutate(
      swe_pctile = purrr::pmap_dbl(list(swe_in, p10, p20, p30, p50, p70, p80, p90), interpolate_pctile),
      precip_pctile = purrr::pmap_dbl(list(precip_in, p10_prc, p20_prc, p30_prc, p50_prc, p70_prc, p80_prc, p90_prc), interpolate_pctile),
      drought_type = dplyr::case_when(
        is.na(swe_pctile) | is.na(precip_pctile) ~ NA_character_,
        swe_pctile >= 30 ~ "No drought",
        swe_pctile < 30 & precip_pctile < 30 ~ "Dry",
        swe_pctile < 30 & precip_pctile >= 50 ~ "Warm",
        swe_pctile < 30 & precip_pctile >= 30 & precip_pctile < 50 ~ "Warm-Dry",
        TRUE ~ NA_character_
      ),
      drought_type = factor(drought_type, levels = c("No drought", "Dry", "Warm-Dry", "Warm"))
    )

  if (!is.null(group_col)) {
    # group_col's original column was never touched (.group was just a copy
    # of it), so select it directly instead of renaming .group back - avoids
    # a duplicate-name collision when group_col already exists in df
    result <- result |>
      dplyr::select(date, dplyr::all_of(group_col), water_year, month_day,
                    swe_in, precip_in, swe_pctile, precip_pctile, drought_type)
  } else {
    result <- result |>
      dplyr::select(date, water_year, month_day,
                    swe_in, precip_in, swe_pctile, precip_pctile, drought_type)
  }

  result
}
