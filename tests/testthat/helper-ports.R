# Made-up inputs for the tests of the functions ported from playground PR #165.

# Monthly visitation to `last_hist` and made-up climate futures after it, in the
# layout the projection methods expect. The year-to-year noise comes from sin(),
# so model fits are repeatable and don't need a random seed.
synthetic_visitation <- function(first_year = 1990, last_hist = 2024,
                                 last_year = 2060,
                                 futures = c("Warm Wet" = "CCSM4.rcp45",
                                             "Hot Dry" = "MIROC5.rcp85")) {
  hist <- data.frame(
    ym = seq(as.Date(paste0(first_year, "-01-01")),
             as.Date(paste0(last_hist, "-12-01")), by = "month"),
    CF = "Historical", GCM = "Historical"
  )
  fut_ym <- seq(as.Date(paste0(last_hist + 1, "-01-01")),
                as.Date(paste0(last_year, "-12-01")), by = "month")
  fut <- do.call(rbind, lapply(names(futures), function(cf) {
    data.frame(ym = fut_ym, CF = cf, GCM = futures[[cf]])
  }))
  out <- rbind(hist, fut)
  year <- as.numeric(format(out$ym, "%Y"))
  month <- as.numeric(format(out$ym, "%m"))
  season <- sin(2 * pi * (month - 4) / 12)
  out$tmean_F <- 45 + 20 * season + 0.03 * (year - first_year) +
    0.5 * sin(year * 1.3) + ifelse(out$CF == "Hot Dry", 2, 0)
  # Growth that levels off, and a seasonal shape that isn't just temperature's
  out$visitors <- ifelse(
    out$CF == "Historical",
    (1000 + 800 / (1 + exp(-0.15 * (year - 2005))) + 30 * sin(year * 2.7)) *
      (2 + season + 0.25 * cos(4 * pi * month / 12)),
    NA
  )
  out
}

# A DEM falling to the south (0.1 m per m), with a valley down x = 500
synthetic_dem <- function() {
  dem <- terra::rast(xmin = 0, xmax = 1000, ymin = 0, ymax = 1000,
                     resolution = 10, crs = "EPSG:5070")
  cells <- seq_len(terra::ncell(dem))
  terra::values(dem) <- 2000 + terra::yFromCell(dem, cells) / 10 +
    abs(terra::xFromCell(dem, cells) - 500) / 20
  dem
}

# A channel with two branches above a confluence at (500, 500). The east branch
# (ending at 800, 950) reaches the highest point of the DEM on this network.
synthetic_flowlines <- function() {
  sf::st_sf(id = 1:3, geometry = sf::st_sfc(
    sf::st_linestring(rbind(c(500, 100), c(500, 500))),
    sf::st_linestring(rbind(c(500, 500), c(200, 900))),
    sf::st_linestring(rbind(c(500, 500), c(800, 950))),
    crs = 5070
  ))
}

skip_without_whitebox <- function(what) {
  skip_if_not(
    whitebox::check_whitebox_binary(),
    paste0(what, ": no WhiteboxTools binary at ", whitebox::wbt_exe_path(),
           "; run whitebox::install_whitebox()")
  )
}
