#' @title Monthly climatology and each month's share of the annual total
#'
#' @description
#' Collapses a daily, sub-monthly or monthly time series to one value per
#' month, then summarizes across years to a calendar-month climatology: the
#' mean, minimum, maximum, standard deviation and coefficient of variation of
#' each month, plus its fraction of the annual total. The result can be split
#' by grouping columns already in the data (e.g. `CF`, `GCM`, or a pre- and
#' post-event `period`) and by named year windows (e.g. a historical and a
#' mid-century window).
#'
#' @details
#' Two steps happen:
#' 1. **Collapse to monthly.** Rows are grouped by year-month (and any
#'    `group_vars`) and reduced to one value per month with
#'    `monthly_agg_fun`. If `df` already has one row per year-month per group,
#'    this step changes nothing.
#' 2. **Summarize across years.** Those monthly values are summarized by
#'    calendar month, within each `group_vars` combination and each window in
#'    `year_windows`. [tidyr::complete()] adds a row for any calendar month
#'    with no data in a group or window, with `NA` values.
#'
#' `fraction` is each month's `mean_val` divided by the sum of `mean_val`
#' within its group and window.
#'
#' @param df Data frame with at least a date column and a numeric value
#'   column. Required.
#' @param date_col String. The name of the date column in `df`. Default:
#'   `"date"`.
#' @param value_col String. The name of the numeric column to summarize.
#'   Required.
#' @param group_vars Character vector or `NULL`. Columns in `df` to group by
#'   as well as month.
#'   * A character vector, e.g. `c("CF", "system")`: one climatology per
#'     combination of those columns, which are kept in the output.
#'   * `NULL`: one climatology for all of `df`.
#'
#'   Default: `NULL`.
#' @param monthly_agg_fun String. How rows within a year-month are collapsed
#'   to one monthly value. There is no default, because the wrong choice
#'   silently changes the units and meaning.
#'   * `"sum"`: for additive fluxes like runoff or water use.
#'   * `"mean"`: for intensities like temperature.
#'   * `"max"`: for a state variable like snowpack.
#'
#'   Required.
#' @param year_windows Named list of length-2 numeric vectors, or `NULL`. The
#'   year windows to compare, e.g.
#'   `list(Historical = c(1980, 2012), Future = c(2035, 2065))`.
#'   * A named list: the climatology is worked out separately in each window,
#'     from its start year up to but not including its end year
#'     (`year >= start & year < end`). The results are stacked, with the
#'     list's names in a `period` column. A window with no rows gives a
#'     warning.
#'   * `NULL`: the climatology uses every year in `df`, and there is no
#'     `period` column.
#'
#'   Default: `NULL`.
#' @param min_n Number. The fewest monthly values a month needs to be
#'   summarized. A month with fewer gets `NA` in `mean_val`, `min_val`,
#'   `max_val`, `sd_val` and `cv_val`. Default: `1`, which filters nothing.
#'
#' @return A tibble with `period` (if `year_windows` is supplied), the
#'   `group_vars` columns, `month` (1 to 12), `month_date` (a `2000-MM-01`
#'   date for plotting), `mean_val`, `min_val`, `max_val`, `sd_val`, `n`,
#'   `cv_val` and `fraction`.
#' @export
#' @seealso [calc_water_year()] for water years, and [project_visitation()],
#'   whose results include monthly proportions of visitation.
#' @examples
#' dates <- seq(as.Date("2000-01-01"), as.Date("2004-12-31"), by = "day")
#' flow <- data.frame(
#'   date = dates,
#'   runoff = 2 + sin(2 * pi * as.numeric(format(dates, "%j")) / 365)
#' )
#'
#' calc_monthly_proportions(flow, value_col = "runoff",
#'                          monthly_agg_fun = "sum")
#'
#' # Compare two year windows
#' calc_monthly_proportions(
#'   flow, value_col = "runoff", monthly_agg_fun = "sum",
#'   year_windows = list(early = c(2000, 2002), late = c(2002, 2005))
#' )

calc_monthly_proportions <- function(df,
                                      date_col = "date",
                                      value_col,
                                      group_vars = NULL,
                                      monthly_agg_fun,
                                      year_windows = NULL,
                                      min_n = 1) {

  monthly_agg_fun <- match.arg(monthly_agg_fun, c("sum", "mean", "max"))
  agg_fn <- switch(monthly_agg_fun,
                   sum  = function(x) sum(x, na.rm = TRUE),
                   mean = function(x) mean(x, na.rm = TRUE),
                   max  = function(x) max(x, na.rm = TRUE))

  if (is.null(group_vars)) group_vars <- character(0)

  if (!date_col %in% names(df)) {
    stop("`date_col` ('", date_col, "') not found in `df`.")
  }
  if (!value_col %in% names(df)) {
    stop("`value_col` ('", value_col, "') not found in `df`.")
  }
  missing_groups <- setdiff(group_vars, names(df))
  if (length(missing_groups) > 0) {
    stop("`group_vars` not found in `df`: ", paste(missing_groups, collapse = ", "))
  }

  group_syms <- rlang::syms(group_vars)

  dat <- df |>
    dplyr::rename(.date = dplyr::all_of(date_col),
                  .value = dplyr::all_of(value_col)) |>
    dplyr::mutate(.ym = lubridate::floor_date(as.Date(.date), "month"))

  # --- Step 1: collapse to one value per year-month (x group_vars) ----------
  monthly_dat <- dat |>
    dplyr::group_by(.ym, !!!group_syms) |>
    dplyr::summarize(.value = agg_fn(.value), .groups = "drop") |>
    dplyr::mutate(year = lubridate::year(.ym),
                  month = lubridate::month(.ym))

  # --- Step 2: summarize across years, by month (x group_vars x period) -----
  summarize_period <- function(dat_sub, period_name = NULL) {

    out <- dat_sub |>
      dplyr::group_by(!!!group_syms, month) |>
      dplyr::summarize(
        mean_val = mean(.value, na.rm = TRUE),
        min_val  = min(.value,  na.rm = TRUE),
        max_val  = max(.value,  na.rm = TRUE),
        sd_val   = stats::sd(.value, na.rm = TRUE),
        n        = sum(!is.na(.value)),
        .groups = "drop"
      ) |>
      dplyr::mutate(
        cv_val = sd_val / mean_val,
        dplyr::across(c(mean_val, min_val, max_val, sd_val, cv_val),
                      ~ ifelse(n < min_n, NA_real_, .x))
      ) |>
      tidyr::complete(month = 1:12, !!!group_syms) |>
      dplyr::mutate(month_date = lubridate::as_date(paste0("2000-", month, "-01")))

    if (!is.null(period_name)) {
      out <- out |>
        dplyr::mutate(period = period_name) |>
        dplyr::relocate(period)
    }

    # fraction = each month's share of its group's (and period's) annual mean
    out |>
      dplyr::group_by(!!!group_syms, dplyr::across(dplyr::any_of("period"))) |>
      dplyr::mutate(fraction = mean_val / sum(mean_val, na.rm = TRUE)) |>
      dplyr::ungroup()
  }

  if (is.null(year_windows)) {

    result <- summarize_period(monthly_dat)

  } else {

    if (!is.list(year_windows) || is.null(names(year_windows)) ||
        any(names(year_windows) == "")) {
      stop("`year_windows` must be a named list of length-2 numeric vectors, ",
           "e.g. list(Historical = c(1980, 2012), Future = c(2035, 2065)).")
    }

    result <- purrr::imap_dfr(year_windows, function(w, nm) {
      if (length(w) != 2) {
        stop("year_windows[['", nm, "']] must have exactly 2 values (start, end).")
      }
      sub <- monthly_dat |> dplyr::filter(year >= w[1], year < w[2])
      if (nrow(sub) == 0) {
        warning("year_windows['", nm, "'] (", w[1], "-", w[2],
                ") matched no rows in `df` -- its output will be all NA.")
      }
      summarize_period(sub, period_name = nm)
    })
  }

  result |>
    dplyr::relocate(dplyr::any_of("period"), dplyr::all_of(group_vars),
                    month, month_date)
}
