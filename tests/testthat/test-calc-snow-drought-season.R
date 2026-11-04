# 25 water years of cool-season days. SWE and precipitation are constant within
# a water year, so their percentile rank comes from `swe_rank` and `precip_rank`
# (1 to 25). The first three years have the lowest SWE and differ in
# precipitation: lowest (Dry), middling (Warm-Dry) and highest (Warm).
snow_years <- function(first_year = 1999) {
  dates <- seq(as.Date(paste0(first_year, "-11-01")),
               as.Date(paste0(first_year + 25, "-05-31")), by = "day")
  dates <- dates[as.numeric(format(dates, "%m")) %in% c(11, 12, 1:5)]
  water_year <- calc_water_year(dates)
  swe_rank <- water_year - first_year
  precip_rank <- swe_rank
  precip_rank[swe_rank == 1] <- 1
  precip_rank[swe_rank == 2] <- 10
  precip_rank[swe_rank == 3] <- 25
  precip_rank[swe_rank == 25] <- 2
  data.frame(date = dates, water_year_check = water_year,
             swe_in = swe_rank, precip_in = precip_rank)
}

test_that("calc_snow_drought_season() classifies from the SWE and precipitation percentiles", {
  dat <- snow_years()
  out <- calc_snow_drought_season(dat)
  expect_named(out, c("date", "water_year", "month_day", "swe_in", "precip_in",
                      "swe_pctile", "precip_pctile", "drought_type"))
  expect_equal(nrow(out), nrow(dat))
  expect_s3_class(out$drought_type, "factor")
  expect_equal(levels(out$drought_type),
               c("No drought", "Dry", "Warm-Dry", "Warm"))
  expect_equal(out$water_year, dat$water_year_check)

  type_of <- function(year) {
    unique(as.character(out$drought_type[out$water_year == year]))
  }
  expect_equal(type_of(2000), "Dry")
  expect_equal(type_of(2001), "Warm-Dry")
  expect_equal(type_of(2002), "Warm")
  expect_equal(type_of(2024), "No drought")
})

test_that("percentiles run from 0 at or below p10 to 100 at or above p90", {
  out <- calc_snow_drought_season(snow_years())
  expect_true(all(out$swe_pctile[out$swe_in == 1] == 0))
  expect_true(all(out$swe_pctile[out$swe_in == 25] == 100))
  # rank 13 of 25 is the median
  expect_true(all(out$swe_pctile[out$swe_in == 13] == 50))
})

test_that("only cool season months are classified, and rows with missing values go", {
  dat <- snow_years()
  extra <- dat[1, ]
  extra$date <- as.Date("2010-07-15")
  dat <- rbind(dat, extra)
  dat$swe_in[2] <- NA
  out <- calc_snow_drought_season(dat)
  expect_false(as.Date("2010-07-15") %in% out$date)
  expect_false(dat$date[2] %in% out$date)
  short <- calc_snow_drought_season(snow_years(), cool_season_months = c(1, 2))
  expect_setequal(unique(format(short$date, "%m")), c("01", "02"))
})

test_that("climatology_filter scores other rows against the chosen rows", {
  hist <- snow_years()
  hist$CF <- "Historical"
  future <- snow_years(2049)
  future$CF <- "Future"
  future$swe_in <- 100
  future$precip_in <- 100
  dat <- rbind(hist, future)
  out <- calc_snow_drought_season(dat, climatology_filter = dat$CF == "Historical")
  # Scored against history, every future value is above the 90th percentile
  scored <- out[out$date > as.Date("2040-01-01"), ]
  expect_equal(nrow(scored), nrow(future))
  expect_true(all(scored$swe_pctile == 100))
  expect_true(all(scored$drought_type == "No drought"))
})

test_that("group_col builds a separate climatology for each group", {
  a <- snow_years()
  a$site <- "a"
  b <- a
  b$site <- "b"
  b$swe_in <- b$swe_in * 1000
  b$precip_in <- b$precip_in * 1000
  out <- calc_snow_drought_season(rbind(a, b), group_col = "site")
  expect_equal(names(out)[1:2], c("date", "site"))
  expect_equal(out$swe_pctile[out$site == "a"], out$swe_pctile[out$site == "b"])
  expect_equal(out$drought_type[out$site == "a"],
               out$drought_type[out$site == "b"])
})

test_that("date, SWE and precipitation columns can have other names", {
  dat <- snow_years()
  renamed <- data.frame(day = dat$date, snow = dat$swe_in, rain = dat$precip_in)
  expect_equal(
    calc_snow_drought_season(renamed, date_col = "day", swe_col = "snow",
                             precip_col = "rain"),
    calc_snow_drought_season(dat)
  )
})

test_that("a short climatology gives a warning", {
  dat <- snow_years()
  dat <- dat[dat$water_year_check <= 2005, ]
  expect_warning(calc_snow_drought_season(dat), "Fewer than 20 years")
  expect_no_warning(calc_snow_drought_season(dat, min_years = 6))
})
