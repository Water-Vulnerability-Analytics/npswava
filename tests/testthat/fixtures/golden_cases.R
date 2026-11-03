# Cases for the golden comparison between the old code in
# nps_water_vulnerability (before the split) and npswava.
#
# Used by make_golden.R, which writes golden/<id>.rds from the old code, and by
# test-golden.R, which checks that npswava returns the identical object.
# `fun`: the npswava function. `old_fun`: its name in the old code, if it
#   was renamed (snake case, issue #4); make_golden.R calls that one.
# `args` must work with both the old and the new function.
# `needs`: "data" = the shared WaVA data folder, "internet" = a live download.
# `reads`: for a "data" case, the file or folder under the data folder it
#   reads; the case skips without it, and its checksum goes in golden_env.csv.
# `downloads`: what an "internet" case fetches, named in its skip reason.
# `keep`: optional function applied to the result before it is saved or
#   compared, for results identical() can't compare as they are.
#
# Not covered, because they can't run in either version right now (see
# README.md): get_pod_california, get_pod_nevada, get_pod_new_mexico,
# get_xy_watersheds, get_aoi_source, get_snotel and get_rockd_aoi.

# Inputs, built here so both runs get the same objects without a download.
# Small boxes inside each park, with the UNIT_CODE column the water rights,
# buildings and suppliers functions select.
golden_box <- function(xmin, ymin, xmax, ymax, unit_code, crs = 4326) {
  sf::st_sf(
    UNIT_CODE = unit_code,
    geometry = sf::st_as_sfc(sf::st_bbox(
      c(xmin = xmin, ymin = ymin, xmax = xmax, ymax = ymax),
      crs = sf::st_crs(crs)
    ))
  )
}
# Zion Canyon, from Springdale to the Temple of Sinawava (HUC4 1501)
zion_aoi <- golden_box(-113.00, 37.18, -112.92, 37.30, "ZION")
# A smaller box around the south entrance, for the slower geology queries
zion_small <- golden_box(-112.99, 37.20, -112.97, 37.22, "ZION")
# Moraine Park and Bear Lake. In NAD83 (EPSG 4269), like the NPS park
# boundaries: get_pod_colorado() needs the aoi in that CRS
romo_aoi <- golden_box(-105.65, 40.30, -105.55, 40.40, "ROMO", crs = 4269)
# The foot of Lake McDonald
glac_aoi <- golden_box(-114.00, 48.50, -113.90, 48.58, "GLAC")
zion_points <- sf::st_as_sf(
  data.frame(site = c("Springdale", "Zion Lodge"),
             lon = c(-112.9877, -112.9570),
             lat = c(37.1889, 37.2505)),
  coords = c("lon", "lat"), crs = 4326
)
# A made-up valley aquifer, about 2 km by 10 km
aquifer_extent <- sf::st_as_sf(sf::st_sfc(
  sf::st_polygon(list(rbind(c(-112.20, 37.60), c(-112.18, 37.60),
                            c(-112.18, 37.69), c(-112.20, 37.69),
                            c(-112.20, 37.60)))),
  crs = 4326
))
# Made-up changes in climate for the 20 NPS models, both scenarios
cf_models <- c("HadGEM2-ES365", "HadGEM2-CC365", "CNRM-CM5", "CCSM4",
               "CanESM2", "MIROC5", "IPSL-CM5A-MR", "NorESM1-M",
               "bcc-csm1-1-m", "IPSL-CM5A-LR", "CSIRO-Mk3-6-0", "BNU-ESM",
               "MRI-CGCM3", "GFDL-ESM2M", "GFDL-ESM2G", "MIROC-ESM", "inmcm4",
               "MIROC-ESM-CHEM", "bcc-csm1-1", "IPSL-CM5B-LR")
cf_deltas <- data.frame(
  GCM = paste0(rep(cf_models, 2), ".", rep(c("rcp45", "rcp85"), each = 20)),
  RCP = rep(c("rcp45", "rcp85"), each = 20),
  delta_tmean = c(1 + (0:19 %% 7) * 0.3, 2 + (0:19 %% 9) * 0.35),
  delta_ppt = ((0:39 * 7) %% 31) - 15
)
cf_args <- list(df = cf_deltas, x = "delta_tmean", y = "delta_ppt",
                x_name = "Temperature", y_name = "Precipitation",
                high_x = "Hot", low_x = "Warm", high_y = "Wet", low_y = "Dry",
                park = "ZION")

golden_cases <- list(
  # No data, no internet
  list(id = "make_ym",
       fun = "make_ym",
       args = list(date = c("2020-06-15", "2020-06-30", "2021-01"))),
  list(id = "make_ym_date",
       fun = "make_ym",
       args = list(date = as.Date(c("2023-10-09", "2024-02-29")))),
  list(id = "calc_water_year",
       fun = "calc_water_year",
       args = list(dates = c("2020-09-30", "2020-10-01", "2021-03-15"))),
  list(id = "calc_water_year_november",
       fun = "calc_water_year",
       args = list(dates = c("2020-10-15", "2020-11-15"),
                   water_year_start_month = 11)),
  # adds a year to every date (a known bug); still compared
  list(id = "calc_water_year_january",
       fun = "calc_water_year",
       args = list(dates = c("2020-01-15", "2020-12-15"),
                   water_year_start_month = 1)),
  list(id = "baseflow_eckhardt",
       fun = "baseflow_eckhardt",
       args = list(q = c(10, 9.5, 9, 30, 22, 15, 12, 10.5, 9.8, 9.2))),
  list(id = "baseflow_eckhardt_set_start",
       fun = "baseflow_eckhardt",
       args = list(q = c(10, 9.5, 9, 30, 22, 15, 12, 10.5, 9.8, 9.2),
                   alpha = 0.99, BFImax = 0.5, bf_0 = 5)),
  # aquifer_volume() turns s2 off before it checks for NA and returns without
  # turning it back on (a known bug), so turn it back on here; otherwise the
  # cases after this one would buffer in degrees
  list(id = "aquifer_volume_na",
       fun = "aquifer_volume",
       args = list(dtw_ft = NA, aquifer_extent = aquifer_extent,
                   full_thickness_ft = 60, well_depth = 60,
                   valley_wall_elev = 30, theta_deg = 10, sy = 0.2),
       keep = function(x) {
         suppressMessages(sf::sf_use_s2(TRUE))
         x
       }),
  # well_depth >= full_thickness_ft - valley_wall_elev, so the partly
  # penetrating branch (which errors, see README.md) doesn't run
  list(id = "aquifer_volume_full_well",
       fun = "aquifer_volume",
       args = list(dtw_ft = 15, aquifer_extent = aquifer_extent,
                   full_thickness_ft = 60, well_depth = 60,
                   valley_wall_elev = 30, theta_deg = 10, sy = 0.2)),

  # Shared data folder
  list(id = "get_overnight_stays",
       fun = "get_overnight_stays",
       old_fun = "getOvernightStays",
       args = list(parks = c("ZION", "ROMO")),
       needs = "data",
       reads = "all/nps_visitor_use_data"),
  list(id = "map_nhdplus_hr",
       fun = "map_nhdplus_hr",
       old_fun = "mapNHDPlusHR",
       args = list(aoi = zion_aoi),
       needs = c("data", "internet"),
       reads = "all/nhdplushr/15/NHDPLUS_H_1501_HU4_GDB.gdb",
       downloads = "HUC4 boundaries (nhdplusTools::get_huc)"),

  # Internet
  list(id = "list_nwis_sites",
       fun = "list_nwis",
       old_fun = "listNWIS",
       args = list(sites = c("09405500", "09406000")),
       needs = "internet",
       downloads = "the NWIS data inventory (dataRetrieval)"),
  list(id = "list_nwis_aoi",
       fun = "list_nwis",
       old_fun = "listNWIS",
       args = list(aoi = zion_aoi, dist = 2000),
       needs = "internet",
       downloads = "the NWIS data inventory (dataRetrieval)"),
  list(id = "list_nwis_state",
       fun = "list_nwis",
       old_fun = "listNWIS",
       args = list(states = "RI"),
       needs = "internet",
       downloads = "the NWIS site list and data inventory for Rhode Island (dataRetrieval)"),
  list(id = "add_gw_meta",
       fun = "add_gw_meta",
       args = list(nwis_groundwater = data.frame(
         site_no = c("371047112592601", "371146112593601")
       )),
       needs = "internet",
       downloads = "NWIS site metadata (dataRetrieval)"),
  list(id = "nldi_finder",
       fun = "nldi_finder",
       args = list(site_no = "09405500"),
       needs = "internet",
       downloads = "a gage basin from the NLDI (nhdplusTools)"),
  list(id = "get_noaa",
       fun = "get_noaa",
       old_fun = "getNOAA",
       args = list(aoi = zion_aoi, park = "ZION", startDate = "2015-01-01",
                   endDate = "2015-12-31",
                   variables = c("PRCP", "TMAX", "TMIN")),
       needs = "internet",
       downloads = "the GHCN-Daily station list and daily data (rnoaa)"),
  list(id = "get_buildings",
       fun = "get_buildings",
       old_fun = "getBuildings",
       args = list(park_boundary = zion_aoi),
       needs = "internet",
       downloads = "every NPS building (ArcGIS Open Data)"),
  list(id = "get_pod_utah",
       fun = "get_pod_utah",
       old_fun = "getPODUtah",
       args = list(aoi = zion_aoi, dist = 1000),
       needs = "internet",
       downloads = "Utah points of diversion (Utah DNR feature service)"),
  list(id = "get_water_suppliers_utah",
       fun = "get_water_suppliers_utah",
       old_fun = "getWaterSuppliersUtah",
       args = list(aoi = zion_aoi, dist = 5000),
       needs = "internet",
       downloads = "Utah water service areas (ArcGIS Open Data)"),
  list(id = "get_water_use_utah",
       fun = "get_water_use_utah",
       old_fun = "getWaterUseUtah",
       args = list(WRID = "1047"),
       needs = "internet",
       downloads = "a public water system page (Utah Division of Water Rights)"),
  list(id = "get_pod_colorado",
       fun = "get_pod_colorado",
       old_fun = "getPODColorado",
       args = list(aoi = romo_aoi),
       needs = "internet",
       downloads = "the Colorado structures file and the CDSS API"),
  list(id = "get_water_rights_montana",
       fun = "get_water_rights_montana",
       old_fun = "getWaterRightsMontana",
       args = list(aoi = glac_aoi, dist = 1000),
       needs = "internet",
       downloads = "the Montana water rights geodatabase (430 MB, Montana State Library)"),
  list(id = "get_climate_historic_polygon",
       fun = "get_climate_historic",
       args = list(sf = zion_small, col_name = "UNIT_CODE",
                   start = "2015-01-01", end = "2015-12-31"),
       needs = "internet",
       downloads = "GridMET (climateR)"),
  list(id = "get_climate_historic_exact",
       fun = "get_climate_historic",
       args = list(sf = zion_small, col_name = "UNIT_CODE",
                   start = "2015-01-01", end = "2015-12-31", exact = TRUE),
       needs = "internet",
       downloads = "GridMET (climateR)"),
  # return_grid = TRUE writes aoi_gridmet_grid to the global environment;
  # compare it too, and remove it
  list(id = "get_climate_historic_points_grid",
       fun = "get_climate_historic",
       args = list(sf = zion_points, col_name = "site",
                   start = "2015-01-01", end = "2015-12-31",
                   return_grid = TRUE),
       needs = "internet",
       downloads = "GridMET (climateR)",
       keep = function(x) {
         grid <- get("aoi_gridmet_grid", envir = globalenv())
         rm("aoi_gridmet_grid", envir = globalenv())
         list(result = x, aoi_gridmet_grid = grid)
       }),
  list(id = "get_climate_future_points",
       fun = "get_climate_future",
       args = list(sf = zion_points, col_name = "site",
                   start = "2040-01-01", end = "2040-12-31",
                   GCM = c("CCSM4.rcp45", "CCSM4.rcp85")),
       needs = "internet",
       downloads = "MACA projections (climateR)"),
  list(id = "get_climate_future_polygon",
       fun = "get_climate_future",
       args = list(sf = zion_small, col_name = "UNIT_CODE",
                   start = "2040-01-01", end = "2040-12-31",
                   GCM = c("CCSM4.rcp45", "CCSM4.rcp85")),
       needs = "internet",
       downloads = "MACA projections (climateR)"),
  list(id = "get_rockd",
       fun = "get_rockd",
       args = list(points = zion_points),
       needs = "internet",
       downloads = "geologic units from the Macrostrat API"),
  list(id = "get_formation_info",
       fun = "get_formation_info",
       args = list(formation_name = "Navajo", rock_type = "sandstone"),
       needs = "internet",
       downloads = "formation units from the Macrostrat API"),
  list(id = "get_surficial_geology",
       fun = "get_surficial_geology",
       args = list(aoi = zion_small),
       needs = "internet",
       downloads = "surficial geology (USGS National Earth Surface feature service)"),
  list(id = "select_cfs_deep_dive",
       fun = "select_cfs_deep_dive",
       args = c(cf_args, list(to_plot = FALSE)),
       needs = "internet",
       downloads = "the GCM skill table (GitHub)"),
  # the ggplot holds environments, so compare what it is built from. The
  # labels are an S7 object whose class holds functions, so keep them as a
  # plain list
  list(id = "select_cfs_deep_dive_plot",
       fun = "select_cfs_deep_dive",
       args = cf_args,
       needs = "internet",
       downloads = "the GCM skill table (GitHub)",
       keep = function(x) {
         list(selected = x[[1]],
              plot_data = x[[2]]$data,
              plot_labels = lapply(x[[2]]$labels, identity),
              plot_layers = vapply(x[[2]]$layers,
                                   function(l) class(l$geom)[1],
                                   character(1)))
       })
)

# Files and folders in the shared data folder that the "data" cases read.
# make_golden.R records their checksums, and test-golden.R reports it when one
# has changed.
golden_data_files <- unique(unlist(lapply(golden_cases, function(case) {
  case$reads
})))

# md5 of a file, or one md5 over every file in a folder (names and contents)
golden_checksum <- function(path) {
  if (!file.exists(path)) return("missing")
  if (!dir.exists(path)) return(unname(tools::md5sum(path)))
  files <- sort(list.files(path, recursive = TRUE, all.files = TRUE))
  sums <- tools::md5sum(file.path(path, files))
  unname(tools::md5sum(bytes = charToRaw(paste(files, sums, collapse = "\n"))))
}

# Check the fields of every case, so a typo (e.g. `need = "data"`) stops here
# instead of silently dropping a skip.
local({
  fields <- c("id", "fun", "old_fun", "args", "needs", "reads", "downloads",
              "keep")
  for (case in golden_cases) {
    id <- if (is.character(case$id) && length(case$id) == 1) case$id else "?"
    bad <- function(...) stop("golden case ", id, ": ", ..., call. = FALSE)
    extra <- setdiff(names(case), fields)
    if (length(extra) > 0) bad("unknown field(s) ", toString(extra))
    if (id == "?") bad("`id` must be one string")
    if (!is.character(case$fun) || length(case$fun) != 1) {
      bad("`fun` must be one function name")
    }
    if (!is.null(case$old_fun) &&
        !(is.character(case$old_fun) && length(case$old_fun) == 1)) {
      bad("`old_fun` must be one function name")
    }
    if (!is.list(case$args)) bad("`args` must be a list")
    if (!is.null(case$needs) && !all(case$needs %in% c("data", "internet"))) {
      bad("`needs` must be \"data\", \"internet\" or both")
    }
    if ("data" %in% case$needs &&
        !(is.character(case$reads) && length(case$reads) == 1)) {
      bad("a \"data\" case needs `reads` (what it reads in the data folder)")
    }
    if (!"data" %in% case$needs && !is.null(case$reads)) {
      bad("`reads` is only for \"data\" cases")
    }
    if ("internet" %in% case$needs &&
        !(is.character(case$downloads) && length(case$downloads) == 1)) {
      bad("an \"internet\" case needs `downloads` (what it fetches)")
    }
    if (!is.null(case$keep) && !is.function(case$keep)) {
      bad("`keep` must be a function")
    }
  }
  ids <- vapply(golden_cases, function(case) case$id, character(1))
  if (anyDuplicated(ids)) {
    stop("golden case ids must be unique: ", toString(ids[duplicated(ids)]),
         call. = FALSE)
  }
})
