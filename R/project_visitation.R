# project_visitation() is a thin dispatcher over named visitation-projection
# METHODS (see visitation_methods at the bottom of this file). Five methods
# are implemented, ordered here from simplest to most complex: "linear_trend"
# (simple/robust linear extrapolation, optionally climate-adjusted),
# "annual_growth_rate" (a simple fixed compounding annual growth rate, no
# statistical fitting or climate dependence -- included because it mirrors
# how parks have historically hand-estimated visitation growth), "gam_logistic"
# (logistic population cap + annual GAM on climate predictors), "arima_climate"
# (ARIMA forecast, optionally with climate variables as external regressors),
# and "fisichelli" (Fisichelli et al. 2015 monthly temperature-response
# method with capacity ceilings). All five
# share the same (df, ...) input contract and the same output shape (a list
# with at least $annual / $monthly / $result, each with visitors_low/mid/high
# + source columns), so they're interchangeable from a caller's perspective --
# pick whichever fits the park's data.
#
# All five also share several internal helper functions (prefixed with `vis_`)
# for input validation, annual aggregation, monthly-proportion calculation,
# and monthly disaggregation, since that scaffolding is identical regardless
# of which model produces the underlying annual projection.
#
# NONE of this has been run against live data -- no R runtime was available
# while writing it. Test each method against a known case before trusting it
# for a report, and watch for model-fitting warnings (nls() convergence,
# auto.arima() order-selection issues, etc.) -- those are the most likely
# things to need per-park tuning.

# ============================================================================
# ---- Shared internal helpers ----------------------------------------------
# ============================================================================

vis_prep_input <- function(df, date_col, visitor_col, climate_vars, scenario_col,
                            group_cols, historical_label, future_end_year) {
  
  if (is.null(climate_vars)) climate_vars <- character(0)
  
  required_cols <- unique(c(date_col, visitor_col, climate_vars, scenario_col, group_cols))
  missing_cols <- setdiff(required_cols, names(df))
  if (length(missing_cols) > 0) {
    stop("df is missing required column(s): ", paste(missing_cols, collapse = ", "))
  }
  
  # ungroup() defensively -- if the caller's df still carries a group_by()
  # from however vis_clim_df/wbm_monthly was assembled (e.g. a grouped
  # mutate() that was never piped through ungroup()), every dplyr verb
  # downstream (including plain mutate() calls in the method bodies, not
  # just group_by()-using helpers) evaluates per-group instead of over the
  # whole table. That's silent almost everywhere in this file EXCEPT when a
  # mutate() column comes from predict()-ing on a newdata frame built from
  # the ungrouped row count -- there the per-group recycling fails loudly
  # ("`pred_raw` must be size 1, not 1800"), which is what surfaces the
  # problem. Stripping any incoming grouping here fixes it at the source for
  # all four methods, not just the one where it happens to throw.
  df <- df |>
    dplyr::ungroup() |>
    dplyr::rename(.ym = dplyr::all_of(date_col),
                  .visitors = dplyr::all_of(visitor_col)) |>
    dplyr::mutate(.visitors = ifelse(.visitors == 0, NA, .visitors),
                  year  = lubridate::year(.ym),
                  month = lubridate::month(.ym))
  
  # Soft check that df is actually monthly (a hard requirement of the shared
  # contract) -- warns rather than errors, since a few legitimate gaps
  # shouldn't necessarily block a run.
  hist_dates <- sort(unique(df$.ym[df[[scenario_col]] == historical_label]))
  if (length(hist_dates) > 1 && any(diff(as.numeric(hist_dates)) > 45)) {
    warning("Historical rows have gaps > 45 days between observations -- ",
            "confirm `df` is a monthly time series as expected.")
  }
  
  list(
    df = df,
    climate_vars = climate_vars,
    hist_monthly = df |> dplyr::filter(.data[[scenario_col]] == historical_label),
    fut_monthly  = df |> dplyr::filter(.data[[scenario_col]] != historical_label,
                                        year <= future_end_year)
  )
}

vis_annual_visitors <- function(hist_monthly, visitor_agg_fun) {
  hist_monthly |>
    dplyr::group_by(year) |>
    dplyr::summarise(.visitors = visitor_agg_fun(.visitors, na.rm = TRUE), .groups = "drop") |>
    dplyr::mutate(.visitors = ifelse(.visitors == 0, NA, .visitors))
}

vis_monthly_proportions <- function(hist_monthly, exclude_years) {
  hist_monthly |>
    dplyr::filter(!year %in% exclude_years) |>
    dplyr::group_by(month) |>
    dplyr::summarize(mean_vis = mean(.visitors, na.rm = TRUE),
                     max_vis  = max(.visitors,  na.rm = TRUE),
                     min_vis  = min(.visitors,  na.rm = TRUE),
                     sd_vis   = stats::sd(.visitors,   na.rm = TRUE),
                     cv_vis   = sd_vis / mean_vis,
                     .groups = "drop") |>
    dplyr::mutate(month_date = lubridate::as_date(paste0("2000-", month, "-01")),
                  fraction = mean_vis / sum(mean_vis))
}

# Monthly proportions computed separately for one or more named year ranges
# (e.g. a historical baseline period vs. a future projection window),
# rather than collapsed into a single fraction across the whole series the
# way vis_monthly_proportions() is. `dat` must have `year`, `month`, and a
# numeric `vis` column -- see vis_proportions_source() below, which builds that
# table from a method's historical + projected monthly output.
# `summary_years` is a named list of numeric year vectors, e.g.
# list(historical = 1980:2012, future = 2035:2064) -- the names become the
# `period` column in the returned (stacked, not nested) tibble.
vis_monthly_proportions_by_period <- function(dat, summary_years) {

  if (is.null(summary_years)) return(NULL)
  if (!is.list(summary_years) || is.null(names(summary_years)) ||
      any(names(summary_years) == "")) {
    stop("`summary_years` must be a named list of year vectors, e.g. ",
         "list(historical = 1980:2012, future = 2035:2064).")
  }

  purrr::imap_dfr(summary_years, function(yrs, period_name) {
    period_dat <- dat |> dplyr::filter(year %in% yrs)

    if (nrow(period_dat) == 0 || all(is.na(period_dat$vis))) {
      warning("summary_years period '", period_name, "' (years ", min(yrs), "-",
              max(yrs), ") has no visitation data in that range -- its monthly ",
              "proportions will be NA. Check that this method's historical/",
              "projected years actually cover that period.")
    }

    period_dat |>
      dplyr::group_by(month) |>
      dplyr::summarize(mean_vis = mean(vis, na.rm = TRUE),
                       max_vis  = max(vis,  na.rm = TRUE),
                       min_vis  = min(vis,  na.rm = TRUE),
                       sd_vis   = stats::sd(vis,   na.rm = TRUE),
                       cv_vis   = sd_vis / mean_vis,
                       .groups = "drop") |>
      tidyr::complete(month = 1:12) |>
      dplyr::mutate(period = period_name,
                    month_date = lubridate::as_date(paste0("2000-", month, "-01")),
                    fraction = mean_vis / sum(mean_vis, na.rm = TRUE)) |>
      dplyr::relocate(period, month, month_date)
  })
}

# Build the table vis_monthly_proportions_by_period() summarizes: true observed
# monthly visitation for historical months (year/month/.visitors, straight
# from vis_prep_input()'s hist_monthly), and the projected monthly envelope
# (visitors_mid) for months at/after obs_cutoff_year. Kept separate from
# vis_disaggregate_monthly()'s output because that function re-derives even
# OBSERVED months as fraction * annual total (so the whole series is built
# the same way) -- for a seasonality summary we want the real observed
# monthly values where they exist, not a fraction-smoothed reconstruction of
# them. `exclude_years` (the same argument every method already takes) is
# applied only to the historical portion, matching how it's used everywhere
# else in this file -- the projected portion has nothing to exclude by
# construction.
vis_proportions_source <- function(hist_monthly, monthly_result, exclude_years = NULL) {
  hist_part <- hist_monthly
  if (!is.null(exclude_years)) {
    hist_part <- hist_part |> dplyr::filter(!year %in% exclude_years)
  }

  dplyr::bind_rows(
    hist_part |> dplyr::transmute(year, month, vis = .visitors),
    monthly_result |>
      dplyr::filter(source == "projected") |>
      dplyr::transmute(year = lubridate::year(ym), month = lubridate::month(ym),
                       vis = visitors_mid)
  )
}

# Aggregate monthly climate_vars to annual using climate_agg_fun (applied
# uniformly to every variable), grouped by whatever grouping columns are
# passed in. With climate_vars = character(0), returns just the grouping
# columns (used by the pure trend/ARIMA methods when run without climate).
vis_annual_climate_agg <- function(data, climate_vars, climate_agg_fun, group_vars) {
  data |>
    dplyr::group_by(dplyr::across(dplyr::all_of(group_vars))) |>
    dplyr::summarise(dplyr::across(dplyr::all_of(climate_vars),
                                   ~ climate_agg_fun(.x, na.rm = TRUE)),
                     .groups = "drop")
}

# Combine observed annual visitation (before obs_cutoff_year) with the
# projected low/mid/high envelope (at/after obs_cutoff_year) into one table.
vis_assemble_annual_result <- function(annual_hist_visitors, annual_envelope, obs_cutoff_year) {
  dplyr::bind_rows(
    annual_hist_visitors |>
      dplyr::filter(year < obs_cutoff_year) |>
      dplyr::transmute(year, visitors_low = .visitors, visitors_mid = .visitors,
                       visitors_high = .visitors, source = "observed"),
    annual_envelope |>
      dplyr::filter(year >= obs_cutoff_year) |>
      dplyr::transmute(year, visitors_low, visitors_mid, visitors_high, source = "projected")
  ) |>
    dplyr::arrange(year)
}

# Disaggregate annual observed + projected visitation to monthly using
# historical monthly proportions. Observed monthly values are used before
# obs_cutoff_year; the annual envelope (scaled by monthly fraction) is used
# after it.
vis_disaggregate_monthly <- function(min_date, annual_hist_visitors, annual_envelope,
                                  monthly_proportions, obs_cutoff_year, future_end_year) {
  
  monthly_scaffold <- data.frame(
    .ym = seq.Date(min_date, lubridate::as_date(paste0(future_end_year, "-12-01")), by = "month")
  ) |>
    dplyr::mutate(year = lubridate::year(.ym), month = lubridate::month(.ym))
  
  # NOTE: annual_hist_visitors contributes .visitors (observed) and
  # annual_envelope contributes visitors_low/mid/high (projected) -- no name
  # collision between the two joins, so no suffix is needed. The mutate()
  # below is self-referencing (RHS reads the just-joined annual value, LHS
  # overwrites it with the blended monthly value) -- standard dplyr, not a bug.
  monthly_scaffold |>
    dplyr::left_join(annual_hist_visitors, by = "year") |>
    dplyr::left_join(annual_envelope |> dplyr::select(year, visitors_low, visitors_mid, visitors_high),
                     by = "year") |>
    dplyr::left_join(monthly_proportions |> dplyr::select(month, fraction), by = "month") |>
    dplyr::mutate(
      visitors_mid  = ifelse(year < obs_cutoff_year, fraction * .visitors, fraction * visitors_mid),
      visitors_high = ifelse(year < obs_cutoff_year, fraction * .visitors, fraction * visitors_high),
      visitors_low  = ifelse(year < obs_cutoff_year, fraction * .visitors, fraction * visitors_low),
      source = ifelse(year < obs_cutoff_year, "observed", "projected")
    ) |>
    dplyr::rename(ym = .ym) |>
    dplyr::select(ym, visitors_low, visitors_mid, visitors_high, source)
}

# ============================================================================
# ---- Method 1: linear_trend -------------------------------------------------
# ============================================================================

#' @title Project park visitation with a linear trend
#'
#' @description
#' Fits `lm(visitors ~ year)`, or `lm(visitors ~ year + climate_vars)` if
#' `climate_vars` is supplied, on annual historical visitation, then
#' extrapolates using prediction intervals for the low and high bounds. There
#' is no population cap, so a linear extrapolation eventually gives implausible
#' values over long horizons. It suits shorter horizons, or a baseline to check
#' [gam_logistic()] against.
#'
#' @details
#' This has not been checked against live data yet (playground PR #165 asks
#' for that review). Test it on a known case before using it in a report.
#'
#' @inheritParams gam_logistic
#' @param climate_vars Character vector or `NULL`. Monthly climate columns to
#'   add as linear predictors, aggregated to annual with `climate_agg_fun`.
#'   * A character vector, e.g. `"tmean_F"`: the model has `year` and these
#'     columns, and is predicted for each future scenario.
#'   * `NULL`: a pure `visitors ~ year` trend, predicted once per future year.
#'
#'   Default: `NULL`.
#' @param exclude_years Integer vector or `NULL`. Calendar years left out of
#'   the monthly proportions. They stay in the `lm()` fit; see
#'   `exclude_years_lm`. Default: `2020`.
#' @param exclude_years_lm Integer vector or `NULL`. Calendar years left out of
#'   the `lm()` fit. Default: `NULL`.
#' @param prediction_level Number. The level of the prediction interval used
#'   for the low and high bounds. Default: `0.95`.
#'
#' @return A list with `result`, `annual`, `monthly` (if `output_resolution`
#'   is `"monthly"`), `monthly_proportions` and `monthly_proportions_summary`
#'   (if `summary_years` is supplied), as in [gam_logistic()], and `lm_model`,
#'   the fitted model.
#' @export
#' @seealso [project_visitation()] to choose a method by name, and
#'   [annual_growth_rate()], [gam_logistic()], [arima_climate()] and
#'   [fisichelli()] for the other methods.
#' @examples
#' # Monthly visitation to 2024, and one made-up climate future to 2060
#' ym <- seq(as.Date("1990-01-01"), as.Date("2060-12-01"), by = "month")
#' year <- as.numeric(format(ym, "%Y"))
#' month <- as.numeric(format(ym, "%m"))
#' vis_clim <- data.frame(
#'   ym = ym,
#'   CF = ifelse(year < 2025, "Historical", "Warm Wet"),
#'   GCM = ifelse(year < 2025, "Historical", "CCSM4.rcp45"),
#'   tmean_F = 45 + 20 * sin(2 * pi * (month - 4) / 12) + 0.03 * (year - 1990),
#'   visitors = ifelse(
#'     year < 2025,
#'     (1000 + 40 * (year - 1990) + 100 * sin(year * 2.7)) *
#'       (2 + sin(2 * pi * (month - 4) / 12)),
#'     NA
#'   )
#' )
#'
#' vis_proj <- linear_trend(vis_clim, visitor_col = "visitors",
#'                          obs_cutoff_year = 2025, future_end_year = 2060)
#' utils::tail(vis_proj$annual)

linear_trend <- function(df,
                         visitor_col,
                         climate_vars = NULL,
                         date_col = "ym",
                         scenario_col = "CF",
                         historical_label = "Historical",
                         group_cols = c("GCM", "CF"),
                         climate_agg_fun = mean,
                         visitor_agg_fun = sum,
                         exclude_years = 2020,
                         exclude_years_lm = NULL,
                         prediction_level = 0.95,
                         obs_cutoff_year = as.numeric(format(Sys.Date(), "%Y")),
                         future_end_year = 2100,
                         output_resolution = c("monthly", "annual"),
                         summary_years = list(historical = 1980:2012, future = 2035:2064)) {
  
  output_resolution <- match.arg(output_resolution)
  prep <- vis_prep_input(df, date_col, visitor_col, climate_vars, scenario_col,
                          group_cols, historical_label, future_end_year)
  climate_vars <- prep$climate_vars
  hist_monthly <- prep$hist_monthly
  fut_monthly  <- prep$fut_monthly
  has_climate  <- length(climate_vars) > 0
  
  annual_hist_visitors <- vis_annual_visitors(hist_monthly, visitor_agg_fun)
  monthly_proportions  <- vis_monthly_proportions(hist_monthly, exclude_years)
  
  ## ---- Fit lm() on historical annual data ------------------------------------
  if (has_climate) {
    annual_hist_climate <- vis_annual_climate_agg(hist_monthly, climate_vars, climate_agg_fun, group_vars = "year")
    lm_dat <- annual_hist_visitors |> dplyr::left_join(annual_hist_climate, by = "year")
  } else {
    lm_dat <- annual_hist_visitors
  }
  
  if (!is.null(exclude_years_lm)) {
    lm_dat <- lm_dat |> dplyr::filter(!year %in% exclude_years_lm)
  }
  
  lm_formula <- stats::reformulate(c("year", climate_vars), response = ".visitors")
  lm_model   <- stats::lm(lm_formula, data = lm_dat)
  
  ## ---- Project forward --------------------------------------------------------
  if (has_climate) {
    newdata <- vis_annual_climate_agg(fut_monthly, climate_vars, climate_agg_fun,
                                   group_vars = unique(c("year", group_cols))) |>
      dplyr::filter(year <= future_end_year)
  } else {
    newdata <- data.frame(year = (max(annual_hist_visitors$year, na.rm = TRUE) + 1):future_end_year)
  }
  
  preds <- stats::predict(lm_model, newdata = newdata, interval = "prediction", level = prediction_level)
  
  annual_proj <- newdata |>
    dplyr::mutate(vis_pred      = pmax(0, preds[, "fit"]),
                  vis_pred_low  = pmax(0, preds[, "lwr"]),
                  vis_pred_high = preds[, "upr"])
  
  annual_envelope <- annual_proj |>
    dplyr::group_by(year) |>
    dplyr::summarize(visitors_mid  = mean(vis_pred,      na.rm = TRUE),
                     visitors_high = max(vis_pred_high,  na.rm = TRUE),
                     visitors_low  = min(vis_pred_low,   na.rm = TRUE),
                     .groups = "drop")
  
  annual_result <- vis_assemble_annual_result(annual_hist_visitors, annual_envelope, obs_cutoff_year)
  
  out <- list(
    annual = annual_result,
    monthly_proportions = monthly_proportions,
    lm_model = lm_model
  )
  
  need_monthly <- output_resolution == "monthly" || !is.null(summary_years)

  if (need_monthly) {
    monthly_result <- vis_disaggregate_monthly(min(prep$df$.ym), annual_hist_visitors, annual_envelope,
                                            monthly_proportions, obs_cutoff_year, future_end_year)
  }

  if (output_resolution == "monthly") {
    out$monthly <- monthly_result
    out$result  <- monthly_result
  } else {
    out$result <- annual_result
  }

  if (!is.null(summary_years)) {
    out$monthly_proportions_summary <- vis_monthly_proportions_by_period(
      vis_proportions_source(hist_monthly, monthly_result, exclude_years),
      summary_years
    )
  }
  
  out
}

# ============================================================================
# ---- Method 2: annual_growth_rate -------------------------------------------
# ============================================================================

#' @title Project park visitation with a fixed annual growth rate
#'
#' @description
#' Grows annual visitation by a constant percentage each year, compounded from
#' an observed annual total:
#' `visitors(year) = base_visitors * (1 + rate) ^ (year - base_year)`. There is
#' no statistical fit, climate dependence or population ceiling. It mirrors how
#' many parks have estimated future visitation by hand.
#'
#' The projection is the same for every climate scenario, by design. It is most
#' useful as a transparent baseline, e.g. to compare a park's traditional 5
#' percent a year with what a climate-informed model projects.
#'
#' @details
#' This has not been checked against live data yet (playground PR #165 asks
#' for that review). It takes the same `df` as the other methods so it can be
#' used through [project_visitation()], but climate columns aren't used.
#'
#' @inheritParams gam_logistic
#' @param growth_rate_mid Number. The fractional annual growth rate of the
#'   central projection, e.g. `0.05` for 5 percent a year. Required, because no
#'   default suits a park-specific assumption like this.
#' @param growth_rate_low Number or `NULL`. The rate for the low bound.
#'   * A number: `visitors_low` is compounded at this rate.
#'   * `NULL`: uses `growth_rate_mid`.
#'
#'   Default: `NULL`.
#' @param growth_rate_high Number or `NULL`. The rate for the high bound.
#'   * A number: `visitors_high` is compounded at this rate.
#'   * `NULL`: uses `growth_rate_mid`.
#'
#'   Default: `NULL`.
#' @param base_year Integer or `NULL`. The historical year to compound from.
#'   * An integer: that year, which must have annual visitation that isn't `NA`
#'     or excluded.
#'   * `NULL`: the most recent such year.
#'
#'   Default: `NULL`.
#' @param exclude_years Integer vector or `NULL`. Calendar years left out of
#'   the monthly proportions and the choice of base year. Default: `NULL`.
#'
#' @return A list with `result`, `annual`, `monthly` (if `output_resolution`
#'   is `"monthly"`), `monthly_proportions` and `monthly_proportions_summary`
#'   (if `summary_years` is supplied), as in [gam_logistic()], and
#'   `growth_rates` (the low, mid and high rates used), `base_year` and
#'   `base_visitors` (the compounding base used).
#' @export
#' @seealso [project_visitation()] to choose a method by name, and
#'   [linear_trend()], [gam_logistic()], [arima_climate()] and [fisichelli()]
#'   for the other methods.
#' @examples
#' # Monthly visitation to 2024, and one made-up climate future to 2060
#' ym <- seq(as.Date("1990-01-01"), as.Date("2060-12-01"), by = "month")
#' year <- as.numeric(format(ym, "%Y"))
#' month <- as.numeric(format(ym, "%m"))
#' vis_clim <- data.frame(
#'   ym = ym,
#'   CF = ifelse(year < 2025, "Historical", "Warm Wet"),
#'   GCM = ifelse(year < 2025, "Historical", "CCSM4.rcp45"),
#'   tmean_F = 45 + 20 * sin(2 * pi * (month - 4) / 12) + 0.03 * (year - 1990),
#'   visitors = ifelse(
#'     year < 2025,
#'     (1000 + 40 * (year - 1990) + 100 * sin(year * 2.7)) *
#'       (2 + sin(2 * pi * (month - 4) / 12)),
#'     NA
#'   )
#' )
#'
#' vis_proj <- annual_growth_rate(vis_clim, visitor_col = "visitors",
#'                                growth_rate_mid = 0.05,
#'                                growth_rate_low = 0.03,
#'                                growth_rate_high = 0.07,
#'                                obs_cutoff_year = 2025,
#'                                future_end_year = 2060)
#' vis_proj$growth_rates
#' utils::tail(vis_proj$annual)

annual_growth_rate <- function(df,
                               visitor_col,
                               date_col = "ym",
                               scenario_col = "CF",
                               historical_label = "Historical",
                               group_cols = c("GCM", "CF"),
                               growth_rate_mid,
                               growth_rate_low = NULL,
                               growth_rate_high = NULL,
                               base_year = NULL,
                               visitor_agg_fun = sum,
                               exclude_years = NULL,
                               obs_cutoff_year = as.numeric(format(Sys.Date(), "%Y")),
                               future_end_year = 2100,
                               output_resolution = c("monthly", "annual"),
                               summary_years = list(historical = 1980:2012, future = 2035:2064)) {

  if (missing(growth_rate_mid) || is.null(growth_rate_mid)) {
    stop("annual_growth_rate(): `growth_rate_mid` is required (e.g. 0.05 for 5%/year growth).")
  }
  if (is.null(growth_rate_low))  growth_rate_low  <- growth_rate_mid
  if (is.null(growth_rate_high)) growth_rate_high <- growth_rate_mid

  output_resolution <- match.arg(output_resolution)
  # climate_vars = NULL -- this method has no climate inputs, but still
  # routes through the same vis_prep_input() contract as every other
  # method so it can be swapped in via project_visitation(method = ...)
  # without the caller needing to change how `df` is built.
  prep <- vis_prep_input(df, date_col, visitor_col, climate_vars = NULL, scenario_col,
                          group_cols, historical_label, future_end_year)
  hist_monthly <- prep$hist_monthly

  annual_hist_visitors <- vis_annual_visitors(hist_monthly, visitor_agg_fun)
  monthly_proportions  <- vis_monthly_proportions(hist_monthly, exclude_years)

  fit_dat <- annual_hist_visitors |>
    dplyr::filter(!year %in% exclude_years) |>
    tidyr::drop_na(.visitors, year)

  if (nrow(fit_dat) == 0) {
    stop("annual_growth_rate(): no valid (non-NA, non-excluded) annual historical ",
         "observations available to use as a compounding base -- check `df` for years ",
         "with no visitor data.")
  }

  if (is.null(base_year)) {
    base_row <- fit_dat |> dplyr::filter(year == max(year))
  } else {
    base_row <- fit_dat |> dplyr::filter(year == base_year)
    if (nrow(base_row) == 0) {
      stop("annual_growth_rate(): `base_year` = ", base_year, " has no valid annual ",
           "historical visitation in `df` (after removing NA/excluded years).")
    }
  }
  base_year_used <- base_row$year[1]
  base_visitors  <- base_row$.visitors[1]

  if (base_year_used >= future_end_year) {
    stop("annual_growth_rate(): the compounding base year (", base_year_used, ") is not ",
         "before `future_end_year` (", future_end_year, ") -- `", base_year_used + 1, ":",
         future_end_year, "` would produce a descending/invalid sequence. Check `base_year` ",
         "and `future_end_year`.")
  }

  future_years <- (base_year_used + 1):future_end_year

  # Climate-blind by design -- the same compounding curve applies to every
  # future year regardless of climate scenario/GCM, so (unlike the other
  # four methods) there is no cross-scenario min/max envelope here; the
  # low/high bounds come purely from growth_rate_low/growth_rate_high.
  annual_envelope <- data.frame(year = future_years) |>
    dplyr::mutate(
      raw_mid  = base_visitors * (1 + growth_rate_mid)  ^ (year - base_year_used),
      raw_low  = base_visitors * (1 + growth_rate_low)  ^ (year - base_year_used),
      raw_high = base_visitors * (1 + growth_rate_high) ^ (year - base_year_used)
    ) |>
    dplyr::transmute(
      year,
      visitors_mid  = raw_mid,
      visitors_low  = pmin(raw_low, raw_high),
      visitors_high = pmax(raw_low, raw_high)
    )

  annual_result <- vis_assemble_annual_result(annual_hist_visitors, annual_envelope, obs_cutoff_year)

  out <- list(
    annual = annual_result,
    monthly_proportions = monthly_proportions,
    growth_rates = c(low = growth_rate_low, mid = growth_rate_mid, high = growth_rate_high),
    base_year = base_year_used,
    base_visitors = base_visitors
  )

  need_monthly <- output_resolution == "monthly" || !is.null(summary_years)

  if (need_monthly) {
    monthly_result <- vis_disaggregate_monthly(min(prep$df$.ym), annual_hist_visitors, annual_envelope,
                                            monthly_proportions, obs_cutoff_year, future_end_year)
  }

  if (output_resolution == "monthly") {
    out$monthly <- monthly_result
    out$result  <- monthly_result
  } else {
    out$result <- annual_result
  }

  if (!is.null(summary_years)) {
    out$monthly_proportions_summary <- vis_monthly_proportions_by_period(
      vis_proportions_source(hist_monthly, monthly_result, exclude_years),
      summary_years
    )
  }
  
  out
}

# ============================================================================
# ---- Method 3: gam_logistic -------------------------------------------------
# ============================================================================

#' @title Project park visitation with logistic growth and a climate GAM
#'
#' @description
#' Fits low, mid and high logistic population-growth curves, capped at a set
#' percentage above the historical maximum, then fits an annual GAM relating
#' visitation to the logistic population term and any climate predictors. It
#' projects that GAM across future climate scenarios, smooths the resulting
#' envelope with logistic curves, and can split the result to months using
#' historical monthly proportions.
#'
#' The population ceiling (`growth_pct`, `low_pct` and `high_pct`) is a hard
#' cap on the final output. The climate-driven projection is smoothed by
#' fitting a logistic curve with K fixed at the ceiling, and that form can't
#' exceed K in any year. A warning is given if the raw GAM projection exceeds
#' K in any year, because the smoothed curve is then saturating rather than
#' tracking the climate signal.
#'
#' @details
#' This has not been checked against live data yet (playground PR #165 asks
#' for that review). Test it on a known case before using it in a report, and
#' watch for [stats::nls()] convergence warnings, which may need per-park
#' tuning of `hist_nls_start` and `proj_nls_start`.
#'
#' `$gam_concurvity` is worth checking whenever the model has smooth climate
#' terms. It is `NULL` for linear terms, because [mgcv::concurvity()] only works
#' on smooths.
#' Concurvity is the GAM analog of multicollinearity. The logistic population
#' term and a climate variable like mean temperature can both trend upward
#' over the same years, so the model may not separate them cleanly. Values near
#' 1 in the "worst" row mean the split between those terms is less trustworthy
#' than the model's R-squared suggests.
#'
#' @param df Data frame. One monthly time series with visitation and climate
#'   already joined: one row per month for the historical period, and one row
#'   per month per climate scenario for the future. Historical rows are those
#'   where `df[[scenario_col]] == historical_label`; every other value is a
#'   future scenario. Visitors can be `NA` in future rows, and zeros are
#'   treated as `NA`. Build it from the full climate series and join visitation
#'   onto that, so future months are kept. Required.
#' @param visitor_col String. The name of the monthly visitor column, e.g.
#'   `"TotalVisitors"`. Required.
#' @param climate_vars Character vector or `NULL`. Monthly climate columns to
#'   use as GAM predictors, aggregated to annual with `climate_agg_fun`.
#'   * A character vector, e.g. `c("tmean_F", "snowpack_in_wbm")`: each is a
#'     predictor alongside the population term.
#'   * `NULL`: a population-only model, `.visitors ~ pop`.
#'
#'   Default: `NULL`.
#' @param date_col String. The name of the monthly date column. Default:
#'   `"ym"`.
#' @param scenario_col String. The column that identifies historical and
#'   future rows. Default: `"CF"`.
#' @param historical_label String. The value of `scenario_col` on historical
#'   rows. Default: `"Historical"`.
#' @param group_cols Character vector. The columns that identify one future
#'   scenario. Default: `c("GCM", "CF")`.
#' @param growth_pct Number. The fraction above the historical maximum that
#'   sets the mid carrying capacity:
#'   `K_mid = max(annual visitors) * (1 + growth_pct)`. Default: `0.5`.
#' @param low_pct Number. The multiplier on `K_mid` for the low carrying
#'   capacity. Default: `0.8`.
#' @param high_pct Number. The multiplier on `K_mid` for the high carrying
#'   capacity. Default: `1.2`.
#' @param climate_agg_fun Function. Aggregates each monthly climate variable to
#'   annual; it must take `na.rm`. Default: `mean`.
#' @param visitor_agg_fun Function. Aggregates monthly visitors to annual; it
#'   must take `na.rm`. Default: `sum`.
#' @param exclude_years Integer vector or `NULL`. Calendar years left out of
#'   the logistic fits and the monthly proportions. Default: `2020`.
#' @param exclude_years_gam Integer vector or `NULL`. Calendar years left out
#'   of the GAM fit only. Default: `NULL`.
#' @param smooth_terms Logical or the string `"auto"`. How climate predictors
#'   enter the GAM. It has no effect without `climate_vars`.
#'   * `TRUE`: each is wrapped in `mgcv::s(..., k = smooth_k)`.
#'   * `FALSE`: each is a linear term.
#'   * `"auto"`: fits both with REML and uses the one with the lower AIC.
#'
#'   Default: `FALSE`.
#' @param smooth_k Integer. The basis dimension of each smooth term. Annual
#'   visitation series are short, and mgcv's own default (`k = 10`) can use
#'   more degrees of freedom than they support. Raise it only if diagnostics
#'   (e.g. `mgcv::gam.check()` on `$gam_model`) show the fit is too constrained.
#'   Default: `4`.
#' @param hist_nls_start Named list. Starting values (`r`, `t0`) for the
#'   historical logistic fits. Default: `list(r = 0.05, t0 = 2010)`.
#' @param proj_nls_start Named list. Starting values (`r`, `t0`) for the
#'   projected logistic fits. Default: `list(r = 0.05, t0 = 2050)`.
#' @param proj_period Numeric vector of length 2. The first and last years
#'   summarized in `$proj_period_summary`. Default: `c(2035, 2064)`.
#' @param obs_cutoff_year Integer. Years before this use observed visitation;
#'   this year and later use the projection. Default: the current year.
#' @param future_end_year Integer. The last year to project. Default: `2100`.
#' @param output_resolution String. What `$result` holds. `df` must be monthly
#'   either way.
#'   * `"monthly"`: the monthly table, also returned as `$monthly`.
#'   * `"annual"`: the annual table.
#'
#'   Default: `"monthly"`.
#' @param park_pal Named list or `NULL`. Colors for the diagnostic plot.
#'   * A list with a `pink` element: that color marks the modeled line.
#'   * `NULL`: the modeled line is `"tomato"`.
#'
#'   Default: `NULL`.
#' @param summary_years Named list of numeric year vectors, or `NULL`. Periods
#'   to summarize monthly seasonality for.
#'   * A named list: `$monthly_proportions_summary` is added, using observed
#'     visitation for months in the historical record and the projected
#'     `visitors_mid` for later months.
#'   * `NULL`: that table is skipped.
#'
#'   Default: `list(historical = 1980:2012, future = 2035:2064)`.
#'
#' @return A list:
#'   * `result`: the table chosen by `output_resolution`.
#'   * `annual`: `year`, `visitors_low`, `visitors_mid`, `visitors_high` and
#'     `source` (`"observed"` or `"projected"`).
#'   * `monthly`: `ym` and the same columns, if `output_resolution` is
#'     `"monthly"`.
#'   * `monthly_proportions`: seasonality across the historical record.
#'   * `monthly_proportions_summary`: the same, per period, if `summary_years`
#'     is supplied.
#'   * `logistic_models`: the `low`, `mid` and `high` nls fits.
#'   * `K_low`, `K_mid`, `K_high`: the population ceilings used.
#'   * `gam_model`, `gam_used_smooth`: the fitted GAM and whether it used
#'     smooth climate terms.
#'   * `gam_concurvity`: from [mgcv::concurvity()], or `NULL` if the model has
#'     no smooth terms.
#'   * `gam_diagnostics`: a panel of diagnostic plots.
#'   * `proj_period_summary`: the mean, maximum and minimum in `proj_period`.
#' @export
#' @seealso [project_visitation()] to choose a method by name, and
#'   [linear_trend()], [annual_growth_rate()], [arima_climate()] and
#'   [fisichelli()] for the other methods.
#' @examples
#' # Monthly visitation to 2024, and one made-up climate future to 2060
#' ym <- seq(as.Date("1990-01-01"), as.Date("2060-12-01"), by = "month")
#' year <- as.numeric(format(ym, "%Y"))
#' month <- as.numeric(format(ym, "%m"))
#' vis_clim <- data.frame(
#'   ym = ym,
#'   CF = ifelse(year < 2025, "Historical", "Warm Wet"),
#'   GCM = ifelse(year < 2025, "Historical", "CCSM4.rcp45"),
#'   tmean_F = 45 + 20 * sin(2 * pi * (month - 4) / 12) + 0.03 * (year - 1990),
#'   visitors = ifelse(
#'     year < 2025,
#'     (1000 + 40 * (year - 1990) + 100 * sin(year * 2.7)) *
#'       (2 + sin(2 * pi * (month - 4) / 12)),
#'     NA
#'   )
#' )
#'
#' vis_proj <- gam_logistic(vis_clim, visitor_col = "visitors",
#'                          climate_vars = "tmean_F", obs_cutoff_year = 2025,
#'                          future_end_year = 2060)
#' vis_proj$K_mid
#' utils::tail(vis_proj$annual)

gam_logistic <- function(df,
                         visitor_col,
                         climate_vars = NULL,
                         date_col = "ym",
                         scenario_col = "CF",
                         historical_label = "Historical",
                         group_cols = c("GCM", "CF"),
                         growth_pct = 0.5,
                         low_pct = 0.8,
                         high_pct = 1.2,
                         climate_agg_fun = mean,
                         visitor_agg_fun = sum,
                         exclude_years = 2020,
                         exclude_years_gam = NULL,
                         smooth_terms = FALSE,
                         smooth_k = 4,
                         hist_nls_start = list(r = 0.05, t0 = 2010),
                         proj_nls_start = list(r = 0.05, t0 = 2050),
                         proj_period = c(2035, 2064),
                         obs_cutoff_year = as.numeric(format(Sys.Date(), "%Y")),
                         future_end_year = 2100,
                         output_resolution = c("monthly", "annual"),
                         park_pal = NULL,
                         summary_years = list(historical = 1980:2012, future = 2035:2064)) {

  output_resolution <- match.arg(output_resolution)
  if (!(isTRUE(smooth_terms) || isFALSE(smooth_terms) || identical(smooth_terms, "auto"))) {
    stop("gam_logistic(): `smooth_terms` must be TRUE, FALSE, or \"auto\" (got: ",
         paste(smooth_terms, collapse = ", "), ").")
  }
  prep <- vis_prep_input(df, date_col, visitor_col, climate_vars, scenario_col,
                          group_cols, historical_label, future_end_year)
  climate_vars <- prep$climate_vars
  hist_monthly <- prep$hist_monthly
  fut_monthly  <- prep$fut_monthly

  accent_col <- if (!is.null(park_pal) && !is.null(park_pal$pink)) park_pal$pink else "tomato"
  
  annual_hist_visitors <- vis_annual_visitors(hist_monthly, visitor_agg_fun)
  annual_hist_climate  <- vis_annual_climate_agg(hist_monthly, climate_vars, climate_agg_fun, group_vars = "year")
  monthly_proportions  <- vis_monthly_proportions(hist_monthly, exclude_years)
  
  ## ---- Logistic growth model (low/mid/high carrying capacity) -------------
  logistic_mod <- function(K, r, t0, year) K / (1 + exp(-r * (year - t0)))
  
  K_mid  <- max(annual_hist_visitors$.visitors, na.rm = TRUE) * (1 + growth_pct)
  K_low  <- K_mid * low_pct
  K_high <- K_mid * high_pct
  
  # algorithm = "port" + explicit lower/upper bounds keeps the fit from
  # wandering into a region where exp() overflows and the Gauss-Newton
  # numericDeriv() step blows up ("Missing value or an infinity produced") --
  # the unconstrained default algorithm is fragile for logistic curves unless
  # the starting values happen to already be close to the solution.
  fit_hist_logistic <- function(K, dat) {
    stats::nls(.visitors ~ logistic_mod(K, r, t0, year), data = dat, start = hist_nls_start,
        algorithm = "port",
        lower = c(r = 1e-4, t0 = min(dat$year) - 50),
        upper = c(r = 5,    t0 = max(dat$year) + 100),
        control = stats::nls.control(maxiter = 200, warnOnly = TRUE))
  }
  
  vis_an_fit <- annual_hist_visitors |>
    dplyr::filter(!year %in% exclude_years) |>
    tidyr::drop_na(.visitors, year)
  
  if (nrow(vis_an_fit) < 3) {
    stop("gam_logistic(): fewer than 3 valid annual observations available for the ",
         "logistic fit after removing NA and excluded years. Check `df` for years with ",
         "no visitor data (a fully-NA/zero year collapses to NA after annual aggregation).")
  }
  
  logistic_low  <- fit_hist_logistic(K_low,  vis_an_fit)
  logistic_mid  <- fit_hist_logistic(K_mid,  vis_an_fit)
  logistic_high <- fit_hist_logistic(K_high, vis_an_fit)
  
  ## ---- Annual GAM: visitors ~ climate + population -------------------------
  gam_dat <- annual_hist_visitors |>
    dplyr::left_join(annual_hist_climate, by = "year") |>
    dplyr::arrange(year) |>
    dplyr::mutate(pop = logistic_mod(K_mid, stats::coef(logistic_mid)["r"], stats::coef(logistic_mid)["t0"], year))
  
  if (!is.null(exclude_years_gam)) {
    gam_dat <- gam_dat |> dplyr::filter(!year %in% exclude_years_gam)
  }
  
  # k is capped (smooth_k, default 4) rather than left at mgcv's default
  # (k = 10) -- annual visitation series are typically only a few dozen
  # points, and an unconstrained s() per climate_vars entry can consume more
  # effective degrees of freedom than a short series can support, especially
  # with more than one climate predictor. method = "REML" (rather than
  # mgcv::gam()'s older GCV.Cp default) gives more stable smoothness
  # selection and is what makes the AIC comparison below valid when
  # smooth_terms = "auto" -- both candidates need to be fit the same way for
  # their AICs to be comparable.
  build_gam_formula <- function(use_smooth) {
    terms <- if (use_smooth && length(climate_vars) > 0) {
      paste0("s(", climate_vars, ", k = ", smooth_k, ")")
    } else {
      climate_vars
    }
    stats::reformulate(c(terms, "pop"), response = ".visitors")
  }

  if (identical(smooth_terms, "auto")) {
    if (length(climate_vars) == 0) {
      # Nothing to smooth -- "auto" degenerates to the linear (pop-only) fit.
      use_smooth <- FALSE
    } else {
      fit_linear <- mgcv::gam(build_gam_formula(FALSE), data = gam_dat, method = "REML")
      fit_smooth <- mgcv::gam(build_gam_formula(TRUE),  data = gam_dat, method = "REML")
      use_smooth <- stats::AIC(fit_smooth) < stats::AIC(fit_linear)
    }
  } else {
    use_smooth <- isTRUE(smooth_terms)
  }

  gam_formula <- build_gam_formula(use_smooth)
  gam_model   <- mgcv::gam(gam_formula, data = gam_dat, method = "REML")

  gam_concurvity <- if (length(climate_vars) > 0) {
    tryCatch(mgcv::concurvity(gam_model, full = TRUE), error = function(e) NULL)
  } else {
    NULL
  }

  gam_dat <- gam_dat |>
    dplyr::mutate(pred_vis = pmax(0, stats::predict(gam_model, newdata = gam_dat)),
                  pred_vis_residual = .visitors - pred_vis)
  
  r2_val <- round(summary(gam_model)$r.sq, 2)
  p_val  <- format.pval(mean(summary(gam_model)$p.pv), digits = 2, eps = 0.001)
  
  gam_diagnostics <- ggpubr::ggarrange(
    ggplot2::ggplot(gam_dat) +
      ggplot2::geom_line(ggplot2::aes(x = year, y = .visitors, color = "Observed"), linewidth = 1) +
      ggplot2::geom_line(ggplot2::aes(x = year, y = pred_vis,  color = "Modeled"),  linewidth = 1) +
      ggplot2::theme_bw() +
      ggplot2::theme(legend.position = c(0.85, 0.25),
            legend.background = ggplot2::element_rect(fill = scales::alpha("white", 0.7), color = NA),
            legend.key = ggplot2::element_blank()) +
      ggplot2::annotate("text",
               label = as.character(as.expression(bquote(R^2 == .(r2_val) ~~~ p < .(p_val)))),
               parse = TRUE, x = -Inf, y = Inf, hjust = -0.1, vjust = 1.5,
               size = 4, fontface = "italic") +
      ggplot2::labs(x = "Year", y = "Annual Visitors", title = "Annual Model Performance") +
      ggplot2::scale_color_manual("", values = c("Modeled" = accent_col, "Observed" = "black")),
    ggplot2::ggplot(gam_dat, ggplot2::aes(x = year, y = pred_vis_residual)) +
      ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
      ggplot2::geom_line() + ggplot2::geom_point() +
      ggplot2::labs(y = "Residual (# visitors)", x = "Year", title = "Annual Residuals") +
      ggplot2::theme_bw(),
    ggplot2::ggplot(gam_dat, ggplot2::aes(x = .visitors, y = pred_vis)) +
      ggplot2::geom_abline(intercept = 0, slope = 1, linetype = "dotted", linewidth = 1) +
      ggplot2::geom_point(alpha = 0.7, size = 2) +
      ggplot2::theme_bw() +
      ggplot2::labs(x = "Observed Annual Visitors", y = "Modeled Annual Visitors", title = "1:1 Accuracy"),
    ncol = 1, align = "hv"
  )
  
  ## ---- Project across future climate scenarios ------------------------------
  # predict.gam() is row-wise and doesn't depend on grouping, so we can call
  # predict() once on the whole future annual table rather than looping per
  # GCM/CF group.
  group_vars_annual  <- unique(c("year", group_cols))
  annual_fut_climate <- vis_annual_climate_agg(fut_monthly, climate_vars, climate_agg_fun,
                                            group_vars = group_vars_annual)
  
  annual_proj <- annual_fut_climate |>
    dplyr::mutate(pop = logistic_mod(K_mid, stats::coef(logistic_mid)["r"], stats::coef(logistic_mid)["t0"], year))
  
  preds <- stats::predict(gam_model, newdata = annual_proj, se.fit = TRUE)
  
  annual_proj <- annual_proj |>
    dplyr::mutate(vis_pred      = as.numeric(preds$fit),
                  vis_pred_se   = as.numeric(preds$se.fit),
                  vis_pred_low  = pmax(0, vis_pred - vis_pred_se * 1.96),
                  vis_pred_high = vis_pred + vis_pred_se * 1.96) |>
    dplyr::group_by(year) |>
    dplyr::mutate(vis_yearly_max  = max(vis_pred_high, na.rm = TRUE),
                  vis_yearly_min  = min(vis_pred_low,  na.rm = TRUE),
                  vis_yearly_mean = mean(c(vis_yearly_max, vis_yearly_min), na.rm = TRUE)) |>
    dplyr::ungroup()
  
  ## ---- Smooth logistic envelopes over the noisy GAM projections -------------
  # K is passed in explicitly (K_low/K_mid/K_high, computed above from
  # growth_pct/low_pct/high_pct) rather than re-derived from the GAM output
  # (as an earlier version did via K_start <- max(y_var)). Fixing K at fit
  # time is what actually makes it a ceiling: the logistic form
  # K/(1+exp(-r*(year-t0))) is mathematically bounded by K for every year, so
  # the smoothed curve can never exceed the population cap you set. Deriving
  # K from the GAM's own predictions instead would just describe whatever
  # the GAM happened to produce -- growth_pct/low_pct/high_pct would then
  # have no real effect on the final output, only on the "pop" covariate fed
  # into the GAM (which the GAM is free to override via the climate terms).
  fit_proj_logistic <- function(y_var, dat, K) {
    if (max(y_var, na.rm = TRUE) > K) {
      warning("gam_logistic(): the GAM's climate-driven projection exceeds the population ",
              "ceiling (K = ", round(K), ") for at least one year -- the smoothed logistic ",
              "curve will saturate at K rather than following the raw GAM values. Consider ",
              "raising growth_pct/high_pct if this isn't intended, or check the climate ",
              "predictors' influence on the GAM.")
    }
    stats::nls(y_var ~ logistic_mod(K, r, t0, year), data = dat, start = proj_nls_start,
        algorithm = "port",
        lower = c(r = 1e-4, t0 = min(dat$year) - 50),
        upper = c(r = 5,    t0 = max(dat$year) + 100),
        control = stats::nls.control(maxiter = 200, warnOnly = TRUE))
  }
  
  annual_envelope <- annual_proj |>
    dplyr::distinct(year, vis_yearly_mean, vis_yearly_max, vis_yearly_min)
  
  if (nrow(annual_envelope) == 0) {
    stop("gam_logistic(): no future climate-scenario rows found in `df` (rows where ",
         "df[[scenario_col]] != historical_label). This commonly happens when `df` is ",
         "built by left_join()-ing a climate table ONTO a visitation table -- if the ",
         "visitation table only has historical months, future rows get dropped by the ",
         "join. Build `df` the other way around: start from the full historical+future ",
         "climate time series and left_join() visitation onto IT, so future months are ",
         "kept (with NA in `visitor_col`).")
  }
  
  log_mid  <- fit_proj_logistic(annual_envelope$vis_yearly_mean, annual_envelope, K = K_mid)
  log_high <- fit_proj_logistic(annual_envelope$vis_yearly_max,  annual_envelope, K = K_high)
  log_low  <- fit_proj_logistic(annual_envelope$vis_yearly_min,  annual_envelope, K = K_low)
  
  annual_envelope <- annual_envelope |>
    dplyr::mutate(visitors_mid  = as.numeric(stats::predict(log_mid,  newdata = annual_envelope)),
                  visitors_high = as.numeric(stats::predict(log_high, newdata = annual_envelope)),
                  visitors_low  = as.numeric(stats::predict(log_low,  newdata = annual_envelope)))
  
  proj_period_summary <- annual_envelope |>
    dplyr::filter(year >= proj_period[1], year <= proj_period[2]) |>
    dplyr::summarize(mean_vis = mean(visitors_mid,  na.rm = TRUE),
                     max_vis  = max(visitors_high, na.rm = TRUE),
                     min_vis  = min(visitors_low,  na.rm = TRUE))
  
  annual_result <- vis_assemble_annual_result(annual_hist_visitors, annual_envelope, obs_cutoff_year)
  
  out <- list(
    annual = annual_result,
    monthly_proportions = monthly_proportions,
    logistic_models = list(low = logistic_low, mid = logistic_mid, high = logistic_high),
    K_low = K_low, K_mid = K_mid, K_high = K_high,
    gam_model = gam_model,
    gam_used_smooth = use_smooth,
    gam_concurvity = gam_concurvity,
    gam_diagnostics = gam_diagnostics,
    proj_period_summary = proj_period_summary
  )
  
  need_monthly <- output_resolution == "monthly" || !is.null(summary_years)

  if (need_monthly) {
    monthly_result <- vis_disaggregate_monthly(min(prep$df$.ym), annual_hist_visitors, annual_envelope,
                                            monthly_proportions, obs_cutoff_year, future_end_year)
  }

  if (output_resolution == "monthly") {
    out$monthly <- monthly_result
    out$result  <- monthly_result
  } else {
    out$result <- annual_result
  }

  if (!is.null(summary_years)) {
    out$monthly_proportions_summary <- vis_monthly_proportions_by_period(
      vis_proportions_source(hist_monthly, monthly_result, exclude_years),
      summary_years
    )
  }
  
  out
}

# ============================================================================
# ---- Method 4: arima_climate -------------------------------------------------
# ============================================================================

#' @title Project park visitation with ARIMA
#'
#' @description
#' Fits an automatically selected ARIMA model ([forecast::auto.arima()]) on
#' annual historical visitation, optionally with annual climate variables as
#' external regressors. It needs the forecast package, which is only suggested
#' by npswava.
#'
#' Unlike [gam_logistic()] and [linear_trend()], `exclude_years` only affects
#' the monthly proportions here, not the ARIMA fit. `auto.arima()`'s order
#' selection isn't reliably robust to internal missing values, so a gap for an
#' excluded year is more likely to break the fit than help it. Filter `df`
#' first if a year has to be removed from the fit.
#'
#' @details
#' This has not been checked against live data yet (playground PR #165 asks
#' for that review). Test it on a known case before using it in a report.
#'
#' @inheritParams gam_logistic
#' @param climate_vars Character vector or `NULL`. Monthly climate columns to
#'   use as external regressors, aggregated to annual with `climate_agg_fun`.
#'   * A character vector, e.g. `"tmean_F"`: one forecast per future scenario
#'     in `group_cols`, because each has its own regressors.
#'   * `NULL`: one forecast, the same for every scenario.
#'
#'   Default: `NULL`.
#' @param exclude_years Integer vector or `NULL`. Calendar years left out of
#'   the monthly proportions only. Default: `2020`.
#' @param forecast_level Number. The prediction interval for the low and high
#'   bounds, as a percentage (`95`, not `0.95`). Default: `95`.
#' @param arima_args Named list. More arguments for
#'   [forecast::auto.arima()], e.g. `list(seasonal = FALSE)`. Default: `list()`.
#'
#' @return A list with `result`, `annual`, `monthly` (if `output_resolution`
#'   is `"monthly"`), `monthly_proportions` and `monthly_proportions_summary`
#'   (if `summary_years` is supplied), as in [gam_logistic()], and
#'   `arima_model`, the fitted model.
#' @export
#' @seealso [project_visitation()] to choose a method by name, and
#'   [linear_trend()], [annual_growth_rate()], [gam_logistic()] and
#'   [fisichelli()] for the other methods.
#' @examplesIf requireNamespace("forecast", quietly = TRUE)
#' # Monthly visitation to 2024, and one made-up climate future to 2060
#' ym <- seq(as.Date("1990-01-01"), as.Date("2060-12-01"), by = "month")
#' year <- as.numeric(format(ym, "%Y"))
#' month <- as.numeric(format(ym, "%m"))
#' vis_clim <- data.frame(
#'   ym = ym,
#'   CF = ifelse(year < 2025, "Historical", "Warm Wet"),
#'   GCM = ifelse(year < 2025, "Historical", "CCSM4.rcp45"),
#'   tmean_F = 45 + 20 * sin(2 * pi * (month - 4) / 12) + 0.03 * (year - 1990),
#'   visitors = ifelse(
#'     year < 2025,
#'     (1000 + 40 * (year - 1990) + 100 * sin(year * 2.7)) *
#'       (2 + sin(2 * pi * (month - 4) / 12)),
#'     NA
#'   )
#' )
#'
#' vis_proj <- arima_climate(vis_clim, visitor_col = "visitors",
#'                           obs_cutoff_year = 2025, future_end_year = 2060)
#' vis_proj$arima_model
#' utils::tail(vis_proj$annual)

arima_climate <- function(df,
                          visitor_col,
                          climate_vars = NULL,
                          date_col = "ym",
                          scenario_col = "CF",
                          historical_label = "Historical",
                          group_cols = c("GCM", "CF"),
                          climate_agg_fun = mean,
                          visitor_agg_fun = sum,
                          exclude_years = 2020,
                          forecast_level = 95,
                          arima_args = list(),
                          obs_cutoff_year = as.numeric(format(Sys.Date(), "%Y")),
                          future_end_year = 2100,
                          output_resolution = c("monthly", "annual"),
                          summary_years = list(historical = 1980:2012, future = 2035:2064)) {
  
  if (!requireNamespace("forecast", quietly = TRUE)) {
    stop("arima_climate() requires the {forecast} package. Install it with ",
         "install.packages('forecast').")
  }
  
  output_resolution <- match.arg(output_resolution)
  prep <- vis_prep_input(df, date_col, visitor_col, climate_vars, scenario_col,
                          group_cols, historical_label, future_end_year)
  climate_vars <- prep$climate_vars
  hist_monthly <- prep$hist_monthly
  fut_monthly  <- prep$fut_monthly
  has_climate  <- length(climate_vars) > 0
  
  annual_hist_visitors <- vis_annual_visitors(hist_monthly, visitor_agg_fun)
  monthly_proportions  <- vis_monthly_proportions(hist_monthly, exclude_years)
  
  ## ---- Build a gap-free annual ts() over the full historical year range -----
  year_range <- min(annual_hist_visitors$year):max(annual_hist_visitors$year)
  
  vis_ts_df <- data.frame(year = year_range) |>
    dplyr::left_join(annual_hist_visitors, by = "year")
  
  vis_ts <- stats::ts(vis_ts_df$.visitors, start = min(year_range), frequency = 1)
  
  if (has_climate) {
    annual_hist_climate <- vis_annual_climate_agg(hist_monthly, climate_vars, climate_agg_fun, group_vars = "year")
    xreg_hist <- data.frame(year = year_range) |>
      dplyr::left_join(annual_hist_climate, by = "year") |>
      dplyr::select(dplyr::all_of(climate_vars)) |>
      as.matrix()
    
    arima_fit <- do.call(forecast::auto.arima, c(list(y = vis_ts, xreg = xreg_hist), arima_args))
  } else {
    arima_fit <- do.call(forecast::auto.arima, c(list(y = vis_ts), arima_args))
  }
  
  ## ---- Project forward ---------------------------------------------------------
  if (has_climate) {
    annual_fut_climate <- vis_annual_climate_agg(fut_monthly, climate_vars, climate_agg_fun,
                                              group_vars = unique(c("year", group_cols))) |>
      dplyr::filter(year <= future_end_year)
    
    annual_proj <- annual_fut_climate |>
      dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) |>
      dplyr::group_split() |>
      purrr::map_dfr(function(this_scenario) {
        this_scenario <- this_scenario |> dplyr::arrange(year)
        xreg_fut <- as.matrix(this_scenario[, climate_vars, drop = FALSE])
        fc <- forecast::forecast(arima_fit, xreg = xreg_fut, level = forecast_level)
        this_scenario |>
          dplyr::mutate(vis_pred      = as.numeric(fc$mean),
                        vis_pred_low  = pmax(0, as.numeric(fc$lower[, 1])),
                        vis_pred_high = as.numeric(fc$upper[, 1]))
      })
  } else {
    future_years <- (max(year_range) + 1):future_end_year
    fc <- forecast::forecast(arima_fit, h = length(future_years), level = forecast_level)
    annual_proj <- data.frame(year = future_years,
                              vis_pred      = as.numeric(fc$mean),
                              vis_pred_low  = pmax(0, as.numeric(fc$lower[, 1])),
                              vis_pred_high = as.numeric(fc$upper[, 1]))
  }
  
  annual_envelope <- annual_proj |>
    dplyr::group_by(year) |>
    dplyr::summarize(visitors_mid  = mean(vis_pred,      na.rm = TRUE),
                     visitors_high = max(vis_pred_high, na.rm = TRUE),
                     visitors_low  = min(vis_pred_low,  na.rm = TRUE),
                     .groups = "drop")
  
  annual_result <- vis_assemble_annual_result(annual_hist_visitors, annual_envelope, obs_cutoff_year)
  
  out <- list(
    annual = annual_result,
    monthly_proportions = monthly_proportions,
    arima_model = arima_fit
  )
  
  need_monthly <- output_resolution == "monthly" || !is.null(summary_years)

  if (need_monthly) {
    monthly_result <- vis_disaggregate_monthly(min(prep$df$.ym), annual_hist_visitors, annual_envelope,
                                            monthly_proportions, obs_cutoff_year, future_end_year)
  }

  if (output_resolution == "monthly") {
    out$monthly <- monthly_result
    out$result  <- monthly_result
  } else {
    out$result <- annual_result
  }

  if (!is.null(summary_years)) {
    out$monthly_proportions_summary <- vis_monthly_proportions_by_period(
      vis_proportions_source(hist_monthly, monthly_result, exclude_years),
      summary_years
    )
  }
  
  out
}

# ============================================================================
# ---- Method 5: fisichelli ---------------------------------------------------
# ============================================================================

#' @title Project park visitation with the Fisichelli et al. (2015) method
#'
#' @description
#' Fits a monthly temperature-visitation curve on long-term calendar-month
#' climatology (one point per month), chooses among null, linear, quadratic and
#' cubic models by BIC, and uses the temperature relationship only if it passes
#' a screen: temperature is in the chosen model, and adjusted R-squared is at
#' least `r2_threshold`. A park that fails the screen carries its historical
#' monthly climatology forward unchanged, keeping the seasonal pattern with no
#' climate effect.
#'
#' It differs from [gam_logistic()], [linear_trend()] and [arima_climate()] in
#' two ways:
#' 1. There is no year-over-year growth term. The model relates monthly
#'    temperature to monthly visitation, so any long-term growth has to be
#'    handled separately.
#' 2. `visitors_low`, `visitors_mid` and `visitors_high` are not statistical
#'    uncertainty bounds. They are the same temperature-driven prediction
#'    passed through three growth ceilings, based on the historically busiest
#'    month, which represent different assumptions about capacity. So
#'    `visitors_low <= visitors_mid <= visitors_high` always holds.
#'
#' @details
#' This has not been checked against live data yet (playground PR #165 asks
#' for that review). The polynomial models are fit on 12 or fewer points, so
#' cubic and quadratic fits have few residual degrees of freedom. A degree is
#' skipped without at least `degree + 2` points, but check
#' `summary(fit$temp_model)` regardless.
#'
#' Unlike the other methods, which split years to months with one fixed
#' fraction per calendar month, this one predicts each future month from its
#' own temperature. So a real shift in seasonality can show up between periods
#' in `$monthly_proportions_summary`.
#'
#' @references
#' Fisichelli, N. A., Schuurman, G. W., Monahan, W. B., & Ziesler, P. S. (2015).
#' Protected area tourism in a changing climate: will visitation at US national
#' parks warm up or overheat? *PLoS ONE*.
#'
#' @inheritParams gam_logistic
#' @param temp_var String. The single monthly temperature column to use as the
#'   predictor. Required.
#' @param degree_max Integer. The highest polynomial degree to try, as well as
#'   the null model. Default: `3`, as in the original method.
#' @param r2_threshold Number. The adjusted R-squared below which the
#'   temperature model is rejected in favor of the historical monthly
#'   climatology. Default: `0.5`, as in the original method.
#' @param cap_low_pct Number. The fraction above the busiest month's historical
#'   mean for the conservative ceiling, used for `visitors_low`. Default:
#'   `0.05`, as in the original method.
#' @param cap_mid_pct Number or `NULL`. The fraction for the middle ceiling,
#'   used for `visitors_mid`.
#'   * A number: used as is.
#'   * `NULL`: the midpoint of `cap_low_pct` and `cap_high_pct`.
#'
#'   Default: `NULL`.
#' @param cap_high_pct Number. The fraction for the generous ceiling, used for
#'   `visitors_high`. Default: `0.25`, as in the original method.
#' @param exclude_years Integer vector or `NULL`. Calendar years left out of
#'   the monthly climatology and the model fit. Default: `NULL`, as the original
#'   method excludes no years.
#'
#' @return A list with `result`, `annual`, `monthly` (if `output_resolution`
#'   is `"monthly"`) and `monthly_proportions_summary` (if `summary_years` is
#'   supplied), as in [gam_logistic()], and:
#'   * `monthly_climatology`: the 12-row table the model is trained on.
#'   * `temp_model`, `model_name`: the chosen lm fit and its name (`"null"`,
#'     `"linear"`, `"quadratic"` or `"cubic"`).
#'   * `temp_sensitive`: whether the screen passed.
#'   * `adj_r2`: the adjusted R-squared, `NA` for the null model.
#'   * `visit_cap_upper`, `visit_cap_mid`, `visit_cap_lower`: the ceilings.
#' @export
#' @seealso [project_visitation()] to choose a method by name, and
#'   [linear_trend()], [annual_growth_rate()], [gam_logistic()] and
#'   [arima_climate()] for the other methods.
#' @examples
#' # Monthly visitation to 2024, and one made-up climate future to 2060
#' ym <- seq(as.Date("1990-01-01"), as.Date("2060-12-01"), by = "month")
#' year <- as.numeric(format(ym, "%Y"))
#' month <- as.numeric(format(ym, "%m"))
#' vis_clim <- data.frame(
#'   ym = ym,
#'   CF = ifelse(year < 2025, "Historical", "Warm Wet"),
#'   GCM = ifelse(year < 2025, "Historical", "CCSM4.rcp45"),
#'   tmean_F = 45 + 20 * sin(2 * pi * (month - 4) / 12) + 0.03 * (year - 1990),
#'   visitors = ifelse(
#'     year < 2025,
#'     (1000 + 40 * (year - 1990) + 100 * sin(year * 2.7)) *
#'       (2 + sin(2 * pi * (month - 4) / 12)),
#'     NA
#'   )
#' )
#'
#' vis_proj <- fisichelli(vis_clim, visitor_col = "visitors",
#'                        temp_var = "tmean_F", obs_cutoff_year = 2025,
#'                        future_end_year = 2060)
#' vis_proj$model_name
#' vis_proj$temp_sensitive
#' utils::tail(vis_proj$annual)

fisichelli <- function(df,
                       visitor_col,
                       temp_var,
                       date_col = "ym",
                       scenario_col = "CF",
                       historical_label = "Historical",
                       group_cols = c("GCM", "CF"),
                       degree_max = 3,
                       r2_threshold = 0.5,
                       cap_low_pct = 0.05,
                       cap_mid_pct = NULL,
                       cap_high_pct = 0.25,
                       exclude_years = NULL,
                       visitor_agg_fun = sum,
                       obs_cutoff_year = as.numeric(format(Sys.Date(), "%Y")),
                       future_end_year = 2100,
                       output_resolution = c("monthly", "annual"),
                       summary_years = list(historical = 1980:2012, future = 2035:2064)) {
  
  if (length(temp_var) != 1) {
    stop("fisichelli(): `temp_var` must name a single temperature column, not a vector.")
  }
  
  # Runs `expr`, and on error re-throws with a stage label prefixed, so a
  # failure says WHICH part of the method broke instead of a bare message
  # bubbling up from deep inside lm()/predict()/dplyr.
  run_stage <- function(label, expr) {
    tryCatch(expr, error = function(e) {
      stop("fisichelli(): failed during '", label, "' -- ", conditionMessage(e), call. = FALSE)
    })
  }
  
  output_resolution <- match.arg(output_resolution)
  prep <- run_stage("input prep", vis_prep_input(df, date_col, visitor_col, temp_var, scenario_col,
                                               group_cols, historical_label, future_end_year))
  hist_monthly <- prep$hist_monthly
  fut_monthly  <- prep$fut_monthly
  
  if (nrow(fut_monthly) == 0) {
    stop("fisichelli(): no future climate-scenario rows found in `df` (rows where ",
         "df[[scenario_col]] != historical_label). See the join-direction note from ",
         "gam_logistic() troubleshooting -- `df` likely needs to be built starting from ",
         "the full historical+future climate series, not from the visitation table.")
  }
  
  ## ---- Long-term monthly climatology (one point per calendar month) ---------
  hist_for_climatology <- if (!is.null(exclude_years)) {
    hist_monthly |> dplyr::filter(!year %in% exclude_years)
  } else {
    hist_monthly
  }
  
  monthly_climatology <- run_stage("climatology construction", {
    mc <- hist_for_climatology |>
      dplyr::group_by(month) |>
      dplyr::summarize(mean_temp     = mean(.data[[temp_var]], na.rm = TRUE),
                       mean_visitors = mean(.visitors, na.rm = TRUE),
                       n_years       = dplyr::n_distinct(year),
                       .groups = "drop")
    
    # Fill any missing calendar months with the across-month mean, so the
    # climatology (and downstream fallback projection) always has 12 rows.
    data.frame(month = 1:12) |>
      dplyr::left_join(mc, by = "month") |>
      dplyr::mutate(mean_visitors = ifelse(is.na(mean_visitors),
                                           mean(mean_visitors, na.rm = TRUE),
                                           mean_visitors))
  })
  
  if (all(is.na(monthly_climatology$mean_temp))) {
    stop("fisichelli(): `temp_var` ('", temp_var, "') is NA for every historical month -- ",
         "check that column in `df` before calling.")
  }
  if (all(is.na(monthly_climatology$mean_visitors))) {
    stop("fisichelli(): `visitor_col` ('", visitor_col, "') is NA for every historical month -- ",
         "check that column in `df` before calling.")
  }
  
  ## ---- Fit null/linear/quadratic/cubic candidates, select by BIC ------------
  fit_dat <- monthly_climatology |> tidyr::drop_na(mean_temp, mean_visitors)
  
  fit_and_select_temp_model <- function(dat, degree_max) {
    n_obs <- nrow(dat)
    degree_names <- c("linear", "quadratic", "cubic")
    
    candidates <- list(null = stats::lm(mean_visitors ~ 1, data = dat))
    for (d in seq_len(degree_max)) {
      if (n_obs >= d + 2) {
        nm <- if (d <= length(degree_names)) degree_names[d] else paste0("degree", d)
        # Bake the literal degree into the formula text (via reformulate())
        # rather than referencing the loop variable `d` inside poly(). If `d`
        # is left as a variable, poly() stores the SYMBOL in the fitted
        # formula, not its value at fit time -- by the time predict() is
        # called later, `d` has already advanced to its final loop value,
        # so e.g. a quadratic model ends up trying to reconstruct a cubic
        # basis ("was fitted with type nmatrix.2 but type nmatrix.3 was
        # supplied"). Literal text in the formula sidesteps that entirely.
        poly_formula <- stats::reformulate(paste0("poly(mean_temp, ", d, ")"), response = "mean_visitors")
        candidates[[nm]] <- tryCatch(stats::lm(poly_formula, data = dat),
                                     error = function(e) NULL)
      }
    }
    candidates <- candidates[!vapply(candidates, is.null, logical(1))]
    bics <- vapply(candidates, stats::BIC, numeric(1))
    best_name  <- names(which.min(bics))
    best_model <- candidates[[best_name]]
    adj_r2 <- if (best_name == "null") NA_real_ else summary(best_model)$adj.r.squared
    
    list(model_name = best_name, model = best_model,
         bic = unname(bics[best_name]), adj_r2 = adj_r2)
  }
  
  fit_result <- run_stage("temperature model fit/selection", fit_and_select_temp_model(fit_dat, degree_max))
  
  temp_in_model  <- fit_result$model_name != "null"
  temp_sensitive <- temp_in_model && !is.na(fit_result$adj_r2) && fit_result$adj_r2 >= r2_threshold
  
  ## ---- Visitation growth ceilings, from the historically busiest month -----
  # arrange()+slice(1) instead of filter(x == max(x)) -- avoids any float
  # equality edge case and always returns exactly one row.
  busiest <- run_stage("busiest-month cap calculation", {
    monthly_climatology |>
      dplyr::arrange(dplyr::desc(mean_visitors)) |>
      dplyr::slice(1)
  })
  
  if (is.null(cap_mid_pct)) cap_mid_pct <- mean(c(cap_low_pct, cap_high_pct))

  cap_upper <- busiest$mean_visitors * (1 + cap_high_pct)
  cap_mid   <- busiest$mean_visitors * (1 + cap_mid_pct)
  cap_lower <- busiest$mean_visitors * (1 + cap_low_pct)

  ## ---- Project forward: apply the model (or climatology fallback) ----------
  fut_monthly <- run_stage("future projection", {
    if (temp_sensitive) {
      newdata_temp <- data.frame(mean_temp = fut_monthly[[temp_var]])
      fut_monthly |>
        dplyr::mutate(pred_raw = pmax(0, stats::predict(fit_result$model, newdata = newdata_temp)))
    } else {
      fut_monthly |>
        dplyr::left_join(monthly_climatology |> dplyr::select(month, mean_visitors), by = "month") |>
        dplyr::mutate(pred_raw = pmax(0, mean_visitors)) |>
        dplyr::select(-mean_visitors)
    }
  })

  # All three of pred_low/pred_mid/pred_high are the SAME raw prediction run
  # through three caps of increasing size (cap_lower <= cap_mid <= cap_upper),
  # and pmin() is monotonic in the cap -- so pred_low <= pred_mid <= pred_high
  # holds pointwise for every row. That ordering survives sum() (monthly ->
  # annual) and, since every per-scenario value is bounded between the
  # cross-scenario min(...low) and max(...high), it survives mean()-vs-min/max
  # across scenarios too. This guarantees visitors_low <= visitors_mid <=
  # visitors_high in the final output -- unlike an earlier version, which
  # used the uncapped pred_raw for the mid column and could show it exceeding
  # the capped high column.
  fut_monthly <- fut_monthly |>
    dplyr::mutate(pred_high = pmin(pred_raw, cap_upper),
                  pred_mid  = pmin(pred_raw, cap_mid),
                  pred_low  = pmin(pred_raw, cap_lower))

  ## ---- Monthly envelope across future climate scenarios ---------------------
  monthly_envelope <- run_stage("monthly envelope assembly", {
    fut_monthly |>
      dplyr::filter(year >= obs_cutoff_year) |>
      dplyr::group_by(year, month) |>
      dplyr::summarize(visitors_mid  = mean(pred_mid,  na.rm = TRUE),
                       visitors_high = max(pred_high, na.rm = TRUE),
                       visitors_low  = min(pred_low,  na.rm = TRUE),
                       .groups = "drop") |>
      dplyr::mutate(ym = lubridate::ym(paste0(year, "-", month)),
                    source = "projected") |>
      dplyr::select(ym, visitors_low, visitors_mid, visitors_high, source)
  })
  
  monthly_hist <- hist_monthly |>
    dplyr::filter(year < obs_cutoff_year) |>
    dplyr::transmute(ym = .ym, visitors_low = .visitors, visitors_mid = .visitors,
                     visitors_high = .visitors, source = "observed")
  
  monthly_result <- dplyr::bind_rows(monthly_hist, monthly_envelope) |>
    dplyr::arrange(ym)
  
  ## ---- Annual: sum monthly within each scenario trajectory first, then -----
  ## ---- take the envelope across scenarios (matches the original's         --
  ## ---- per-park/gcm/ssp/run annual sum, then cross-scenario summary).      --
  annual_by_scenario <- run_stage("annual aggregation by scenario", {
    fut_monthly |>
      dplyr::filter(year >= obs_cutoff_year) |>
      dplyr::group_by(dplyr::across(dplyr::all_of(group_cols)), year) |>
      dplyr::summarize(annual_mid  = sum(pred_mid,  na.rm = TRUE),
                       annual_high = sum(pred_high, na.rm = TRUE),
                       annual_low  = sum(pred_low,  na.rm = TRUE),
                       n_months    = dplyr::n(),
                       .groups = "drop") |>
      dplyr::filter(n_months == 12)  # drop partial years (e.g. a truncated final year)
  })

  if (nrow(annual_by_scenario) == 0) {
    stop("fisichelli(): every future year had fewer than 12 months of data per scenario ",
         "after aggregation, so none survived the complete-year filter -- check `df` for ",
         "gaps in the future climate rows (e.g. a truncated final year, or group_cols not ",
         "uniquely identifying each scenario trajectory).")
  }

  annual_envelope <- annual_by_scenario |>
    dplyr::group_by(year) |>
    dplyr::summarize(visitors_mid  = mean(annual_mid,  na.rm = TRUE),
                     visitors_high = max(annual_high, na.rm = TRUE),
                     visitors_low  = min(annual_low,  na.rm = TRUE),
                     .groups = "drop")
  
  annual_hist_visitors <- vis_annual_visitors(hist_monthly, visitor_agg_fun)
  annual_result <- vis_assemble_annual_result(annual_hist_visitors, annual_envelope, obs_cutoff_year)
  
  out <- list(
    annual = annual_result,
    monthly_climatology = monthly_climatology,
    temp_model = fit_result$model,
    model_name = fit_result$model_name,
    temp_sensitive = temp_sensitive,
    adj_r2 = fit_result$adj_r2,
    visit_cap_upper = cap_upper,
    visit_cap_mid   = cap_mid,
    visit_cap_lower = cap_lower
  )
  
  if (output_resolution == "monthly") {
    out$monthly <- monthly_result
    out$result  <- monthly_result
  } else {
    out$result <- annual_result
  }

  if (!is.null(summary_years)) {
    out$monthly_proportions_summary <- vis_monthly_proportions_by_period(
      vis_proportions_source(hist_monthly, monthly_result, exclude_years),
      summary_years
    )
  }
  
  out
}

# ============================================================================
# ---- Method registry + dispatcher ------------------------------------------
# ============================================================================

visitation_methods <- list(
  linear_trend       = linear_trend,
  annual_growth_rate = annual_growth_rate,
  gam_logistic       = gam_logistic,
  arima_climate      = arima_climate,
  fisichelli         = fisichelli
  # Add future methods here, e.g.:
  # prophet_climate = prophet_climate
)

#' @title Project park visitation with a method chosen by name
#'
#' @description
#' Calls one of the visitation projection methods by name. They take the same
#' monthly `df` and return the same `$result`, `$annual` and `$monthly` tables,
#' each with `visitors_low`, `visitors_mid`, `visitors_high` and `source`, so
#' they can be swapped. The other arguments differ:
#' [fisichelli()] takes `temp_var` (one column) instead of `climate_vars`, and
#' [annual_growth_rate()] takes `growth_rate_mid` and no climate argument.
#'
#' @details
#' These methods have not been checked against live data yet (playground PR
#' #165 asks for that review).
#'
#' @param df Data frame. One monthly time series; see [gam_logistic()].
#'   Required.
#' @param ... Arguments for the chosen method.
#' @param method String. The method to use.
#'   * `"linear_trend"`: [linear_trend()], a linear extrapolation.
#'   * `"annual_growth_rate"`: [annual_growth_rate()], a fixed compounding rate.
#'   * `"gam_logistic"`: [gam_logistic()], a logistic population cap and a
#'     climate GAM.
#'   * `"arima_climate"`: [arima_climate()], an ARIMA forecast.
#'   * `"fisichelli"`: [fisichelli()], a monthly temperature response with
#'     capacity ceilings.
#'
#'   Default: `"gam_logistic"`.
#'
#' @return The list returned by the chosen method. See its help page.
#' @export
#' @seealso [get_overnight_stays()] for park visitor use, and
#'   [calc_monthly_proportions()] for monthly proportions of other series.
#' @examples
#' # Monthly visitation to 2024, and one made-up climate future to 2060
#' ym <- seq(as.Date("1990-01-01"), as.Date("2060-12-01"), by = "month")
#' year <- as.numeric(format(ym, "%Y"))
#' month <- as.numeric(format(ym, "%m"))
#' vis_clim <- data.frame(
#'   ym = ym,
#'   CF = ifelse(year < 2025, "Historical", "Warm Wet"),
#'   GCM = ifelse(year < 2025, "Historical", "CCSM4.rcp45"),
#'   tmean_F = 45 + 20 * sin(2 * pi * (month - 4) / 12) + 0.03 * (year - 1990),
#'   visitors = ifelse(
#'     year < 2025,
#'     (1000 + 40 * (year - 1990) + 100 * sin(year * 2.7)) *
#'       (2 + sin(2 * pi * (month - 4) / 12)),
#'     NA
#'   )
#' )
#'
#' vis_proj <- project_visitation(vis_clim, visitor_col = "visitors",
#'                                method = "linear_trend",
#'                                obs_cutoff_year = 2025,
#'                                future_end_year = 2060)
#' utils::tail(vis_proj$annual)

project_visitation <- function(df, ..., method = "gam_logistic") {
  if (!method %in% names(visitation_methods)) {
    stop("Unknown visitation projection method: '", method, "'. Available: ",
         paste(names(visitation_methods), collapse = ", "))
  }
  visitation_methods[[method]](df, ...)
}
