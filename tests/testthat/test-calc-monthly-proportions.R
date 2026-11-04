# Two years of daily values: 1 a day in 2001, 3 a day in 2002
daily <- function() {
  dates <- seq(as.Date("2001-01-01"), as.Date("2002-12-31"), by = "day")
  data.frame(date = dates,
             value = ifelse(format(dates, "%Y") == "2001", 1, 3))
}

test_that("calc_monthly_proportions() sums to months, then summarizes across years", {
  out <- calc_monthly_proportions(daily(), value_col = "value",
                                  monthly_agg_fun = "sum")
  expect_named(out, c("month", "month_date", "mean_val", "min_val", "max_val",
                      "sd_val", "n", "cv_val", "fraction"))
  expect_equal(nrow(out), 12)
  jan <- out[out$month == 1, ]
  expect_equal(jan$min_val, 31)
  expect_equal(jan$max_val, 93)
  expect_equal(jan$mean_val, 62)
  expect_equal(jan$sd_val, stats::sd(c(31, 93)))
  expect_equal(jan$cv_val, stats::sd(c(31, 93)) / 62)
  expect_equal(jan$n, 2)
  expect_equal(jan$month_date, as.Date("2000-01-01"))
  # the mean year totals 365 * 2 = 730
  expect_equal(jan$fraction, 62 / 730)
  expect_equal(sum(out$fraction), 1)
})

test_that("monthly_agg_fun chooses how days collapse to months", {
  dat <- daily()
  mean_out <- calc_monthly_proportions(dat, value_col = "value",
                                       monthly_agg_fun = "mean")
  expect_equal(mean_out$mean_val, rep(2, 12))
  max_out <- calc_monthly_proportions(dat, value_col = "value",
                                      monthly_agg_fun = "max")
  expect_equal(max_out$max_val, rep(3, 12))
  expect_error(calc_monthly_proportions(dat, value_col = "value"))
  expect_error(calc_monthly_proportions(dat, value_col = "value",
                                        monthly_agg_fun = "median"))
})

test_that("group_vars gives one climatology per group", {
  dat <- rbind(cbind(daily(), site = "a"), cbind(daily(), site = "b"))
  dat$value[dat$site == "b"] <- dat$value[dat$site == "b"] * 10
  out <- calc_monthly_proportions(dat, value_col = "value",
                                  group_vars = "site", monthly_agg_fun = "sum")
  expect_equal(names(out)[1:3], c("site", "month", "month_date"))
  expect_equal(nrow(out), 24)
  expect_equal(out$mean_val[out$site == "b"], out$mean_val[out$site == "a"] * 10)
  expect_equal(sum(out$fraction[out$site == "a"]), 1)
  expect_equal(sum(out$fraction[out$site == "b"]), 1)
})

test_that("year_windows include the start year and exclude the end year", {
  out <- calc_monthly_proportions(
    daily(), value_col = "value", monthly_agg_fun = "sum",
    year_windows = list(first = c(2001, 2002), second = c(2002, 2003))
  )
  expect_equal(names(out)[1:2], c("period", "month"))
  expect_equal(out$mean_val[out$period == "first" & out$month == 1], 31)
  expect_equal(out$mean_val[out$period == "second" & out$month == 1], 93)
  expect_equal(unique(out$n), 1)
})

test_that("a window with no rows warns and is all NA", {
  warned <- capture_warnings(
    out <- calc_monthly_proportions(
      daily(), value_col = "value", monthly_agg_fun = "sum",
      year_windows = list(none = c(1950, 1960))
    )
  )
  expect_match(warned, "matched no rows", all = FALSE)
  expect_equal(nrow(out), 12)
  expect_true(all(is.na(out$mean_val)))
})

test_that("min_n blanks months with too few values", {
  out <- calc_monthly_proportions(daily(), value_col = "value",
                                  monthly_agg_fun = "sum", min_n = 3)
  expect_true(all(is.na(out$mean_val)))
  expect_equal(unique(out$n), 2)
})

test_that("months with no data still get a row", {
  dat <- daily()
  dat <- dat[format(dat$date, "%m") != "07", ]
  out <- calc_monthly_proportions(dat, value_col = "value",
                                  monthly_agg_fun = "sum")
  expect_equal(out$month, 1:12)
  expect_true(is.na(out$mean_val[7]))
  expect_equal(sum(out$fraction, na.rm = TRUE), 1)
})

test_that("calc_monthly_proportions() names what it can't find", {
  dat <- daily()
  expect_error(calc_monthly_proportions(dat, date_col = "day", value_col = "value",
                                        monthly_agg_fun = "sum"), "day")
  expect_error(calc_monthly_proportions(dat, value_col = "flow",
                                        monthly_agg_fun = "sum"), "flow")
  expect_error(calc_monthly_proportions(dat, value_col = "value",
                                        group_vars = "site",
                                        monthly_agg_fun = "sum"), "site")
  expect_error(calc_monthly_proportions(dat, value_col = "value",
                                        monthly_agg_fun = "sum",
                                        year_windows = list(c(2001, 2002))),
               "named list")
  expect_error(calc_monthly_proportions(dat, value_col = "value",
                                        monthly_agg_fun = "sum",
                                        year_windows = list(a = 2001)),
               "exactly 2 values")
})
