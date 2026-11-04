expect_projection_shape <- function(out, first_year = 1990, last_year = 2060,
                                    cutoff = 2025, ordered = TRUE) {
  expect_named(out$annual, c("year", "visitors_low", "visitors_mid",
                             "visitors_high", "source"))
  expect_named(out$monthly, c("ym", "visitors_low", "visitors_mid",
                              "visitors_high", "source"))
  expect_identical(out$result, out$monthly)
  expect_equal(range(out$annual$year), c(first_year, last_year))
  expect_equal(unique(out$annual$source[out$annual$year < cutoff]), "observed")
  expect_equal(unique(out$annual$source[out$annual$year >= cutoff]), "projected")
  projected <- out$annual[out$annual$source == "projected", ]
  if (ordered) {
    expect_true(all(projected$visitors_low <= projected$visitors_mid))
    expect_true(all(projected$visitors_mid <= projected$visitors_high))
  }
  expect_true(all(projected$visitors_low >= 0))
  expect_equal(out$monthly$ym[nrow(out$monthly)],
               as.Date(paste0(last_year, "-12-01")))
}

run_args <- list(visitor_col = "visitors", obs_cutoff_year = 2025,
                 future_end_year = 2060)

test_that("project_visitation() calls the method it names", {
  dat <- synthetic_visitation()
  expect_identical(
    project_visitation(dat, visitor_col = "visitors", obs_cutoff_year = 2025,
                       future_end_year = 2060, method = "linear_trend")$annual,
    linear_trend(dat, visitor_col = "visitors", obs_cutoff_year = 2025,
                 future_end_year = 2060)$annual
  )
  expect_error(project_visitation(dat, visitor_col = "visitors", method = "prophet"),
               "Unknown visitation projection method: 'prophet'.*fisichelli")
})

test_that("annual_growth_rate() compounds from the base year", {
  dat <- synthetic_visitation()
  out <- do.call(annual_growth_rate,
                 c(list(dat, growth_rate_mid = 0.05, growth_rate_low = 0.03,
                        growth_rate_high = 0.07), run_args))
  expect_projection_shape(out)
  annual_obs <- tapply(dat$visitors[dat$CF == "Historical"],
                       format(dat$ym[dat$CF == "Historical"], "%Y"), sum)
  expect_equal(out$base_year, 2024)
  expect_equal(out$base_visitors, annual_obs[["2024"]])
  expect_equal(out$growth_rates, c(low = 0.03, mid = 0.05, high = 0.07))
  row_2030 <- out$annual[out$annual$year == 2030, ]
  expect_equal(row_2030$visitors_mid, annual_obs[["2024"]] * 1.05^6)
  expect_equal(row_2030$visitors_low, annual_obs[["2024"]] * 1.03^6)
  expect_equal(row_2030$visitors_high, annual_obs[["2024"]] * 1.07^6)
})

test_that("annual_growth_rate() uses the mid rate for missing bounds, and a chosen base year", {
  dat <- synthetic_visitation()
  out <- do.call(annual_growth_rate,
                 c(list(dat, growth_rate_mid = 0.02, base_year = 2010), run_args))
  expect_equal(out$base_year, 2010)
  projected <- out$annual[out$annual$source == "projected", ]
  expect_equal(projected$visitors_low, projected$visitors_mid)
  expect_equal(projected$visitors_high, projected$visitors_mid)
  expect_equal(projected$visitors_mid[projected$year == 2025],
               out$base_visitors * 1.02^15)
})

test_that("annual_growth_rate() needs a rate and a usable base year", {
  dat <- synthetic_visitation()
  expect_error(annual_growth_rate(dat, visitor_col = "visitors"),
               "growth_rate_mid.*required")
  expect_error(annual_growth_rate(dat, visitor_col = "visitors",
                                  growth_rate_mid = 0.05, base_year = 1900),
               "`base_year` = 1900")
  expect_error(annual_growth_rate(dat, visitor_col = "visitors",
                                  growth_rate_mid = 0.05, future_end_year = 2020),
               "not before `future_end_year`")
})

test_that("linear_trend() extrapolates a straight line", {
  dat <- synthetic_visitation()
  # A straight line of annual totals, with a little noise so intervals exist
  year <- as.numeric(format(dat$ym, "%Y"))
  dat$visitors <- ifelse(dat$CF == "Historical",
                         (1000 + 10 * (year - 1990) + sin(year * 2.7)) / 12, NA)
  out <- do.call(linear_trend, c(list(dat, exclude_years = NULL), run_args))
  expect_projection_shape(out)
  expect_s3_class(out$lm_model, "lm")
  expect_equal(stats::coef(out$lm_model)[["year"]], 10, tolerance = 0.01)
  fit <- stats::coef(out$lm_model)
  expect_equal(out$annual$visitors_mid[out$annual$year == 2050],
               fit[["(Intercept)"]] + fit[["year"]] * 2050)
})

test_that("linear_trend() takes climate predictors and predicts each scenario", {
  dat <- synthetic_visitation()
  out <- do.call(linear_trend, c(list(dat, climate_vars = "tmean_F"), run_args))
  expect_projection_shape(out)
  expect_true("tmean_F" %in% names(stats::coef(out$lm_model)))
})

test_that("exclude_years_lm leaves years out of the fit only", {
  dat <- synthetic_visitation()
  out <- do.call(linear_trend, c(list(dat, exclude_years_lm = 2000:2005), run_args))
  expect_false(any(out$lm_model$model$year %in% 2000:2005))
  expect_true(all(2000:2005 %in% out$annual$year))
})

test_that("gam_logistic() returns its fits, and the ceilings cap the result", {
  dat <- synthetic_visitation()
  # This made-up climate pushes the raw projection past the low ceiling
  warned <- capture_warnings(
    out <- do.call(gam_logistic, c(list(dat, climate_vars = "tmean_F"), run_args))
  )
  expect_match(warned, "exceeds the population ceiling \\(K = ", all = TRUE)
  expect_projection_shape(out)
  annual_obs <- tapply(dat$visitors[dat$CF == "Historical"],
                       format(dat$ym[dat$CF == "Historical"], "%Y"), sum)
  expect_equal(out$K_mid, max(annual_obs) * 1.5)
  expect_equal(out$K_low, out$K_mid * 0.8)
  expect_equal(out$K_high, out$K_mid * 1.2)
  projected <- out$annual[out$annual$source == "projected", ]
  expect_lte(max(projected$visitors_mid), out$K_mid)
  expect_lte(max(projected$visitors_low), out$K_low)
  expect_lte(max(projected$visitors_high), out$K_high)
  expect_s3_class(out$gam_model, "gam")
  expect_named(out$logistic_models, c("low", "mid", "high"))
  expect_false(out$gam_used_smooth)
  expect_s3_class(out$gam_diagnostics, "ggplot")
  expect_named(out$proj_period_summary, c("mean_vis", "max_vis", "min_vis"))
})

test_that("gam_logistic() runs without climate, and chooses smooth terms when asked", {
  dat <- synthetic_visitation()
  warned <- capture_warnings(pop_only <- do.call(gam_logistic, c(list(dat), run_args)))
  expect_match(warned, "exceeds the population ceiling", all = TRUE)
  expect_projection_shape(pop_only, ordered = FALSE)
  expect_null(pop_only$gam_concurvity)

  auto <- suppressWarnings(
    do.call(gam_logistic, c(list(dat, climate_vars = "tmean_F",
                                 smooth_terms = "auto"), run_args))
  )
  expect_type(auto$gam_used_smooth, "logical")

  smooth <- suppressWarnings(
    do.call(gam_logistic, c(list(dat, climate_vars = "tmean_F",
                                 smooth_terms = TRUE), run_args))
  )
  expect_true(smooth$gam_used_smooth)
  expect_true(is.matrix(smooth$gam_concurvity))
  expect_equal(length(smooth$gam_model$smooth), 1)

  expect_error(do.call(gam_logistic, c(list(dat, smooth_terms = "yes"), run_args)),
               "must be TRUE, FALSE, or")
})

test_that("gam_logistic() explains a data frame with no future rows", {
  dat <- synthetic_visitation()
  dat <- dat[dat$CF == "Historical", ]
  # The empty future table also warns on its way to the error
  expect_error(suppressWarnings(do.call(gam_logistic, c(list(dat), run_args))),
               "no future climate-scenario rows")
})

test_that("arima_climate() forecasts with and without regressors", {
  skip_if_not(requireNamespace("forecast", quietly = TRUE),
              "arima_climate() test: the forecast package isn't installed (it is only in Suggests)")
  dat <- synthetic_visitation()
  no_climate <- do.call(arima_climate, c(list(dat), run_args))
  expect_projection_shape(no_climate)
  expect_s3_class(no_climate$arima_model, "Arima")

  climate <- do.call(arima_climate, c(list(dat, climate_vars = "tmean_F"), run_args))
  expect_projection_shape(climate)
  expect_true("xreg" %in% names(climate$arima_model) ||
                "tmean_F" %in% names(stats::coef(climate$arima_model)))
})

test_that("arima_climate() says when forecast is missing", {
  local_mocked_bindings(requireNamespace = function(...) FALSE, .package = "base")
  expect_error(arima_climate(synthetic_visitation(), visitor_col = "visitors"),
               "requires the \\{forecast\\} package")
})

test_that("fisichelli() uses temperature when it passes the screen", {
  dat <- synthetic_visitation()
  out <- do.call(fisichelli, c(list(dat, temp_var = "tmean_F"), run_args))
  expect_projection_shape(out)
  expect_true(out$temp_sensitive)
  expect_true(out$model_name %in% c("linear", "quadratic", "cubic"))
  expect_s3_class(out$temp_model, "lm")
  expect_gte(out$adj_r2, 0.5)
  expect_equal(nrow(out$monthly_climatology), 12)
})

test_that("fisichelli() caps are based on the historically busiest month", {
  dat <- synthetic_visitation()
  out <- do.call(fisichelli, c(list(dat, temp_var = "tmean_F"), run_args))
  busiest <- max(out$monthly_climatology$mean_visitors)
  expect_equal(out$visit_cap_lower, busiest * 1.05)
  expect_equal(out$visit_cap_mid, busiest * 1.15)
  expect_equal(out$visit_cap_upper, busiest * 1.25)
  projected <- out$monthly[out$monthly$source == "projected", ]
  expect_lte(max(projected$visitors_low), out$visit_cap_lower)
  expect_lte(max(projected$visitors_mid), out$visit_cap_mid)
  expect_lte(max(projected$visitors_high), out$visit_cap_upper)

  chosen <- do.call(fisichelli, c(list(dat, temp_var = "tmean_F", cap_mid_pct = 0.2),
                                  run_args))
  expect_equal(chosen$visit_cap_mid, busiest * 1.2)
})

test_that("fisichelli() carries the climatology forward when the screen fails", {
  dat <- synthetic_visitation()
  out <- do.call(fisichelli, c(list(dat, temp_var = "tmean_F", r2_threshold = 1.01),
                               run_args))
  expect_false(out$temp_sensitive)
  projected <- out$monthly[out$monthly$source == "projected", ]
  month <- as.numeric(format(projected$ym, "%m"))
  # The ceilings don't bite on the mid case below the busiest month
  below <- month != which.max(out$monthly_climatology$mean_visitors)
  expect_equal(projected$visitors_mid[below],
               out$monthly_climatology$mean_visitors[month[below]])
})

test_that("fisichelli() checks its temperature column and future rows", {
  dat <- synthetic_visitation()
  expect_error(do.call(fisichelli, c(list(dat, temp_var = c("a", "b")), run_args)),
               "must name a single temperature column")
  expect_error(do.call(fisichelli, c(list(dat[dat$CF == "Historical", ],
                                          temp_var = "tmean_F"), run_args)),
               "no future climate-scenario rows")
  dat$tmean_F[dat$CF == "Historical"] <- NA
  expect_error(do.call(fisichelli, c(list(dat, temp_var = "tmean_F"), run_args)),
               "is NA for every historical month")
})

test_that("the methods treat zero visitors as missing and ignore grouping", {
  dat <- synthetic_visitation()
  grouped <- dplyr::group_by(dat, GCM)
  expect_identical(
    do.call(linear_trend, c(list(grouped), run_args))$annual,
    do.call(linear_trend, c(list(dat), run_args))$annual
  )
  zero <- dat
  zero$visitors[zero$ym == as.Date("2000-07-01")] <- 0
  gap <- dat
  gap$visitors[gap$ym == as.Date("2000-07-01")] <- NA
  expect_identical(
    do.call(linear_trend, c(list(zero), run_args))$annual,
    do.call(linear_trend, c(list(gap), run_args))$annual
  )
})

test_that("the methods report missing columns and gaps", {
  dat <- synthetic_visitation()
  expect_error(linear_trend(dat, visitor_col = "guests"),
               "missing required column\\(s\\): guests")
  expect_error(linear_trend(dat, visitor_col = "visitors", climate_vars = "ppt"),
               "missing required column\\(s\\): ppt")
  gappy <- dat[!(dat$CF == "Historical" & format(dat$ym, "%Y") == "2000"), ]
  expect_warning(do.call(linear_trend, c(list(gappy), run_args)), "gaps > 45 days")
})

test_that("output_resolution and summary_years choose the tables returned", {
  dat <- synthetic_visitation()
  annual <- do.call(linear_trend, c(list(dat, output_resolution = "annual"), run_args))
  expect_identical(annual$result, annual$annual)
  expect_null(annual$monthly)

  summary <- do.call(linear_trend, c(list(dat), run_args))$monthly_proportions_summary
  expect_setequal(unique(summary$period), c("historical", "future"))
  expect_equal(nrow(summary), 24)
  expect_equal(sum(summary$fraction[summary$period == "future"]), 1)

  none <- do.call(linear_trend, c(list(dat, summary_years = NULL), run_args))
  expect_null(none$monthly_proportions_summary)

  expect_error(do.call(linear_trend, c(list(dat, summary_years = list(1980:2012)),
                                       run_args)),
               "must be a named list")
  warned <- capture_warnings(
    do.call(linear_trend, c(list(dat, summary_years = list(old = 1800:1810)), run_args))
  )
  expect_match(warned, "no visitation data", all = FALSE)
})
