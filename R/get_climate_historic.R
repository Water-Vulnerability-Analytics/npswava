#' @title Download daily historical GridMET climate for points or polygons
#'
#' @description
#' Downloads daily GridMET climate with [climateR::getGridMET()] for each
#' feature of an sf object, so it needs internet, and returns a long table
#' of precipitation and temperature per grid cell and day.
#'
#' * Polygons: every GridMET cell in the polygon, one row per cell and day,
#'   with the cell center as `x` and `y` (see `exact`).
#' * Points: the cell containing the point, one row per day, with the point
#'   (its centroid) as `x` and `y`.
#'
#' Temperatures are converted from kelvin to degrees C (`tmax_C`, `tmin_C`
#' and `tmean_C`, their mean) and precipitation is returned as `ppt_mm`.
#' This needs `vars` to include `"tmmx"`, `"tmmn"` and `"pr"`.
#'
#' @param sf sf object. The areas of interest, all polygons/multipolygons or
#'   all points. Stops with an error for other geometry types. Required.
#' @param col_name Column name, bare or as a string. The column of `sf` that
#'   identifies each feature; it is kept in the output under the same name.
#'   Required.
#' @param start String, `"YYYY-MM-DD"`. The first day to download. Default:
#'   `"1979-01-01"`.
#' @param end String, `"YYYY-MM-DD"`. The last day to download. Default:
#'   `"2023-12-31"`.
#' @param vars Character vector. The GridMET variables to download. The
#'   unit conversions need `"tmmx"`, `"tmmn"` and `"pr"`; others (e.g.
#'   `"srad"`) are kept under their GridMET names for polygons. Default:
#'   `c("tmmx", "tmmn", "pr")`.
#' @param exact Logical. How polygons are sampled; ignored for points.
#'   * `TRUE`: downloads a 4 km buffer around each polygon so edge cells are
#'     included, and keeps every cell that touches the polygon, with
#'     `area_weight`, the fraction of the cell inside it.
#'   * `FALSE`: keeps only the cells whose center is inside the polygon;
#'     `area_weight` is `NA`.
#'
#'   Default: `FALSE`.
#' @param return_grid Logical. Whether to also make the GridMET cells as
#'   polygons.
#'   * `TRUE`: creates `aoi_gridmet_grid` in the global environment, an sf
#'     object of the cells that intersect each feature (in the GridMET CRS),
#'     with the `col_name` column.
#'   * `FALSE`: creates nothing.
#'
#'   Default: `FALSE`.
#'
#' @return A tibble, one row per feature, cell and day, with `x`, `y`,
#'   `date`, `ppt_mm`, `tmax_C`, `tmin_C`, `tmean_C`, `area_weight` and the
#'   `col_name` column.
#' @export
#' @seealso [get_climate_future()] for MACA projections at the same places;
#'   [select_cfs_deep_dive()] to pick climate futures.
#' @examplesIf interactive()
#' # Downloads from the GridMET server, so it needs internet
#' site <- sf::st_as_sf(data.frame(site = "Springdale", lon = -112.9877, lat = 37.1889),
#'                      coords = c("lon", "lat"), crs = 4326)
#' clim <- get_climate_historic(site, col_name = site,
#'                              start = "2020-01-01", end = "2020-01-31")
#' head(clim)

get_climate_historic <- function(sf,
                                 col_name,
                                 start = "1979-01-01",
                                 end = "2023-12-31",
                                 vars = c("tmmx", "tmmn", "pr"),
                                 exact = FALSE,
                                 return_grid = FALSE) {
  
  # 1. Setup and Rename ID column
  sf <- sf %>% dplyr::rename("join_index" = {{col_name}})
  all_climate_data <- vector("list", length = nrow(sf))
  
  if (return_grid) {
    grids <- vector("list", length = nrow(sf))
    names(grids) <- sf$join_index
  }
  
  geom_type <- unique(as.character(sf::st_geometry_type(sf)))
  
  # POLYGON WORKFLOW
  if (any(geom_type %in% c("POLYGON", "MULTIPOLYGON"))) {
    
    for (i in 1:nrow(sf)) {
      aoi <- sf[i,]
      message(paste0('Downloading GridMET for ', aoi$join_index, "..."))
      
      # Download logic
      aoi_fetch <- if(exact) sf::st_buffer(aoi, 4000) else aoi
      clim <- climateR::getGridMET(AOI = aoi_fetch, varname = vars, 
                                   startDate = start, endDate = end)
      
      # Determine CRS and transform AOI
      clim_crs <- terra::crs(clim[[1]])
      aoi_trans <- if(sf::st_crs(aoi) != clim_crs) sf::st_transform(aoi, crs = clim_crs) else aoi
      
      # POPULATE GRID 
      if (return_grid) {
        grids[[i]] <- terra::as.polygons(clim[[1]], values = FALSE, dissolve = FALSE, na.rm = TRUE) %>%
          sf::st_as_sf() %>%
          .[aoi_trans, ]
      }
      
      # Processing Data
      if (!is.data.frame(clim) || inherits(clim[[1]], "SpatRaster")) {
        if (exact) {
          # Weighted Extraction
          all_climate_data[[i]] <- clim %>%
            purrr::map(~terra::extract(., terra::vect(aoi_trans), xy = TRUE, touches = TRUE, weights = TRUE)) %>%
            purrr::map_dfr(~ terra::as.data.frame(., xy = TRUE)) %>%
            data.table::data.table() %>%
            tidyr::pivot_longer(-c(x, y, ID, weight), names_to = "var_temp", values_to = "val") %>%
            tidyr::separate_wider_delim(var_temp, "_", names = c("var", "date")) %>%
            tidyr::drop_na(val) %>%
            dplyr::group_by(x, y, date) %>%
            tidyr::pivot_wider(names_from = "var", values_from = "val") %>%
            dplyr::mutate(date = as.Date(date),
                          ppt_mm = pr,
                          tmax_C = tmmx - 273.15,
                          tmin_C = tmmn - 273.15,
                          tmean_C = (tmax_C + tmin_C)/2,
                          join_index = aoi$join_index) %>%
            dplyr::rename(area_weight = weight) %>%
            dplyr::select(-dplyr::any_of(c("ID", "tmmx", "tmmn", "pr")))
          
        } else {
          # Standard Crop/Mask
          all_climate_data[[i]] <- clim %>%
            purrr::map(~terra::crop(., terra::vect(aoi_trans), mask = TRUE)) %>%         
            purrr::map_dfr(~ terra::as.data.frame(., xy = TRUE)) %>%
            data.table::data.table() %>%
            tidyr::pivot_longer(-c(x, y), names_to = "var_temp", values_to = "val") %>%
            tidyr::separate_wider_delim(var_temp, "_", names = c("var", "date")) %>%
            tidyr::drop_na(val) %>%
            dplyr::group_by(x, y, date) %>%
            tidyr::pivot_wider(names_from = "var", values_from = "val") %>%
            dplyr::mutate(date = as.Date(date),
                          ppt_mm = if("pr" %in% names(.)) {pr} else {NA_real_},
                          tmax_C = if("tmmx" %in% names(.)) {tmmx - 273.15} else {NA_real_},
                          tmin_C = if("tmmn" %in% names(.)) {tmmn - 273.15} else {NA_real_},
                          tmean_C = (tmax_C + tmin_C)/2,
                          join_index = aoi$join_index,
                          area_weight = NA) %>%
            dplyr::select(-dplyr::any_of(c("tmmx", "tmmn", "pr")))
        }
      }
    }
    
    # POINTS WORKFLOW
  } else if (any(geom_type == "POINT")) {
    
    for (i in 1:nrow(sf)) {
      aoi <- sf[i,]
      message(paste0('Downloading GridMET for ', aoi$join_index, "..."))
      
      clim <- climateR::getGridMET(AOI = aoi, varname = vars, 
                                   startDate = start, endDate = end)
      
      if (return_grid) {
        # Fetch small grid for point visualization
        point_grid_raw <- climateR::getGridMET(AOI = sf::st_buffer(aoi, 4000), varname = "tmmx", 
                                               startDate = start, endDate = start)
        grids[[i]] <- terra::as.polygons(point_grid_raw[[1]], values = FALSE, dissolve = FALSE) %>%
          sf::st_as_sf() %>%
          .[sf::st_transform(aoi, terra::crs(point_grid_raw[[1]])), ]
      }
      
      all_climate_data[[i]] <- clim %>%
        data.table::data.table() %>%
        dplyr::mutate(x = sf::st_coordinates(sf::st_centroid(aoi))[[1]],
                      y = sf::st_coordinates(sf::st_centroid(aoi))[[2]],
                      date = as.Date(date),
                      ppt_mm = if("pr" %in% names(.)) {pr} else {NA_real_},
                      tmax_C = if("tmmx" %in% names(.)) {tmmx - 273.15} else {NA_real_},
                      tmin_C = if("tmmn" %in% names(.)) {tmmn - 273.15} else {NA_real_},
                      tmean_C = (tmax_C + tmin_C)/2,
                      join_index = aoi$join_index,
                      area_weight = NA) %>%
        dplyr::select(-dplyr::any_of(c("tmmx", "tmmn", "pr")))
    }
    
  } else {
    stop("Input geometry must be Polygon or Point.")
  }
  
  # FINAL COMBINATION & ASSIGNMENT (POINTS & POLYGONS)
  
  # Combine into one dataframe
  final_df <- dplyr::bind_rows(all_climate_data)
  
  # Restore the original ID column name
  # We use the quosure name to restore the user's specific column name
  id_col_str <- rlang::as_name(rlang::enquo(col_name))
  colnames(final_df)[colnames(final_df) == "join_index"] <- id_col_str
  
  # Global Grid Assignment
  if (return_grid) {
    message("Writing 'aoi_gridmet_grid' to Global Environment.")
    # Bind the grids and add the ID column back
    grid_out <- dplyr::bind_rows(grids, .id = "temp_id")
    colnames(grid_out)[colnames(grid_out) == "temp_id"] <- id_col_str
    assign("aoi_gridmet_grid", grid_out, envir = .GlobalEnv)
  }
  
  return(final_df)
}
