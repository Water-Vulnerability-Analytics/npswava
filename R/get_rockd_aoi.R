#' @title Geologic map units over an area (Macrostrat)
#'
#' @description
#' Gets the Macrostrat geologic map units (<https://macrostrat.org/>) over
#' an area, so it needs internet. The API only answers point queries, so the
#' function puts a 100 x 100 grid of points over `aoi`, drops points outside
#' the US (using [tigris::states()], with a small inland buffer because
#' coastal points often fail), and queries the points one at a time with
#' [get_rockd()]. After each query it drops the remaining points that the
#' returned units already cover. A point often has units from several map
#' sources; only the source at `resolution` is kept, or the next coarser one
#' if it isn't available (with a message at the end).
#'
#' It turns off s2 geometry with [sf::sf_use_s2()] while it runs (the
#' geologic polygons are often invalid) and turns it back on at the end. It
#' shows a progress bar and can take a long time for large areas or fine
#' resolutions.
#'
#' @param aoi sf object. The area of interest, as polygons. Required.
#' @param crop Logical. Whether to cut the units to the area.
#'   * `TRUE`: crops the result to the bounding box of `aoi`.
#'   * `FALSE`: returns the whole units, which can extend far beyond it.
#'
#'   Default: `TRUE`.
#' @param resolution String. The map scale to keep, one of:
#'   * `"tiny"`: global maps, coarsest and quickest;
#'   * `"small"`: national or regional maps;
#'   * `"medium"`: state maps;
#'   * `"large"`: local (e.g. quadrangle) maps, finest and slowest.
#'
#'   Pass one value. Default: `c("tiny", "small", "medium", "large")`, which
#'   stops with an error, so a value must be given.
#'
#' @return An sf object of geologic unit polygons (CRS 4326) with the
#'   Macrostrat columns; the age columns (`b_age`, `t_age`, `b_int_id`,
#'   `t_int_id`, `b_int_age`, `t_int_age`) are character.
#' @export
#' @seealso [get_rockd()] for points; [get_surficial_geology()] for USGS
#'   surficial geology.
#' @examplesIf interactive()
#' # Queries the Macrostrat API many times, so it needs internet and takes a
#' # few minutes even for a small area
#' box <- sf::st_as_sf(sf::st_as_sfc(sf::st_bbox(
#'   c(xmin = -112.99, ymin = 37.20, xmax = -112.95, ymax = 37.24), crs = 4326
#' )))
#' geology <- get_rockd_aoi(box, resolution = "medium")
#' unique(geology$name)

get_rockd_aoi <- function(aoi, crop = TRUE, resolution = c("tiny", "small", "medium", "large")) {
  
  # #' Function to convert aoi to gridded points
  # #' @param polygon = polygon area to make points in
  # #' @param cellsize = size of cells to divide polygon and place point at center.
  # poly_to_points <- function(polygon, cellsize) {
  #   
  #   # Create a grid covering the extent of the polygon
  #   grid <- st_make_grid(st_bbox(polygon), cellsize = cellsize)
  #   
  #   # Intersect the grid with the polygon
  #   grid_intersect <- suppressWarnings(st_intersection(grid, polygon)) %>%
  #     suppressMessages()
  #   
  #   # Convert to sf POINTS object
  #   grid_points <- st_centroid(grid_intersect) %>%
  #     suppressMessages()
  #   
  #   return(grid_points)
  # } # CM commented out in case we need to return to/reference this
  
  sf::sf_use_s2(FALSE) # KEC note - there are too many complex geometries to have 
  # this on. Many sf objects lead to errors when set to true, even with
  # st_make_valid(). Not sure if there are other suggestions, but for now,
  # setting to FALSE while running.  

  # Get map of us for removing points outside of boundary (errors). Add a small
  # buffer because coastal points often return errors because of slight offsets
  # between geologic map and actual coastline.
  us_states_sf <- tigris::states(cb = TRUE, class = "sf") %>%
    dplyr::summarize() %>%
    sf::st_buffer(-.008)
  
  # Track if a fallback resolution is used
  resolution_fallback_used <- FALSE
  resolution_used <- NULL
  
  # Define function to pull rockd sources for a given retrieval of get_rockd().
  # This function takes a list of all the rockd "source_id" strings and 
  # selects the one with "medium" resolution. This can be added as an 
  # argument, potentially within the parent function call.
  
  get_sources_rockd <- function(get_rockd_result, res = resolution) {
    resolution_levels <- c("large", "medium", "small", "tiny")
    resolution_priority <- resolution_levels[which(resolution_levels == res):length(resolution_levels)]
    
    # map_sources <- rmacrostrat::def_sources(source_id = geo_1$source_id) %>%
    #  dplyr::filter(area == min(area, na.rm = TRUE))
    
    # Get character string of available map sources
    map_sources_str <- get_rockd_result[["source_id"]] %>%
      toString() %>%
      gsub(" ", "", .) %>%
      paste(collapse = ",")
    
    # Download map source metadata to allow selection of resolution
    map_sources <- RCurl::getURL(paste0("https://macrostrat.org/api/v2/defs/sources?source_id=",
                                        map_sources_str,
                                        "&format=geojson_bare"),
                                 .opts = RCurl::curlOptions(ssl.verifypeer = FALSE, verbose = FALSE)) %>%
      geojsonsf::geojson_wkt()
    
    for (r in resolution_priority) {
      filtered_sources <- dplyr::filter(map_sources, scale == r)
      if (nrow(filtered_sources) > 0) {
        if (r != res) {
          resolution_fallback_used <<- TRUE
          resolution_used <<- r
        }
        return(filtered_sources[["source_id"]])
      }
    }
    
    stop("No resolution available for the AOI.")
  }
  if(terra::crs(us_states_sf) != terra::crs(aoi)){
    
    aoi <- aoi %>% sf::st_transform(terra::crs(us_states_sf))
  
  }
  # Create grid of points within AOI
  aoi_points <- sf::st_make_grid(aoi, what = "centers", n = c(100, 100))
  
  # Now, filter to points within US (minus buffer)
  intersection_indices <- sf::st_intersects(aoi_points, us_states_sf)
  points_in_polygon_logical <- lengths(intersection_indices) > 0
  aoi_points <- aoi_points[points_in_polygon_logical]
  
  # Get length of points for tracking progress  
  tp <- length(aoi_points)
  
  # Pull geology for first point
  geo_1 <- get_rockd(aoi_points[1])
  
  # Transform aoi_points to crs of geo_1
  aoi_points <- aoi_points %>% 
    sf::st_transform(sf::st_crs(geo_1))
  
  source <- get_sources_rockd(geo_1)
  
  # Filter geo_1 to only include geologic layers from that source
  geo_1 <- geo_1 %>%
    dplyr::filter(source_id == source) %>%
    sf::st_make_valid() %>%
    dplyr::mutate(
      b_age = as.character(b_age),
      b_int_id = as.character(b_int_id),
      b_int_age = as.character(b_int_age),
      t_int_age = as.character(t_int_age),
      t_int_id = as.character(t_int_id),
      t_age = as.character(t_age)
    )
  
  # Select all points that don't overlap with geo_1 and subsequently pull
  # geology for those points, repeating this selection process
  new_aoi_points <- aoi_points %>%
    sf::st_intersects(geo_1) %>%
    lengths() %>%
    `==`(0) %>%
    aoi_points[.]
  
  
  # Pt num is the point number for existing points in the grid. Sometimes,
  # the first search point fails for whatever reason. The code just moves 
  # to the adjacent point when that's the case.
  start_pt_num <- 1 # base point index that subsequent code will reset to
  pt_num <- start_pt_num  # assign to updatable point index
  
  pb <- utils::txtProgressBar(min = 0, max = tp, style = 3, char = "=")
  
  while (length(new_aoi_points) > 0) {
    
    # Save number of remaining points at start of loop
    num_pts_start_loop <- length(new_aoi_points)
    
    # Error check -- if pt_index is greater than # pts, reset
    if (pt_num > num_pts_start_loop) {
      pt_num <- start_pt_num
    }
    
    # Get geology for current pt_num
    geo_2 <- get_rockd(new_aoi_points[pt_num])
    
    if (nrow(geo_2) > 0) {
      source <- get_sources_rockd(geo_2)
      geo_2 <- geo_2 %>%
        dplyr::filter(source_id == source) %>%
        sf::st_make_valid() %>%
        dplyr::mutate(
          b_age = as.character(b_age),
          b_int_id = as.character(b_int_id),
          b_int_age = as.character(b_int_age),
          t_int_age = as.character(t_int_age),
          t_int_id = as.character(t_int_id),
          t_age = as.character(t_age)
        )
    }
    
    # Remove the point that was just tested
    new_aoi_points <- new_aoi_points[-pt_num]
    
    # Remove any other points that intersect the newly returned geologic extent
    new_aoi_points <- new_aoi_points %>%
      sf::st_intersects(geo_2) %>%
      lengths() %>%
      `==`(0) %>%
      new_aoi_points[.]
    
    # Bind geo_2 to geo_1
    geo_1 <- dplyr::bind_rows(geo_1, geo_2)
    
    # Calculate number of points left after removing points
    num_pts_end_loop <- length(new_aoi_points)
    
    # Check if the number of remaining points went down. If not, bump up index 
    # and search next point.
    if (num_pts_end_loop == num_pts_start_loop) {
      pt_num <- pt_num + 1
    } else if (num_pts_end_loop < num_pts_start_loop) {
      pt_num <- start_pt_num
    }
    
    #  Update the progress bar
    utils::setTxtProgressBar(pb, tp - num_pts_end_loop)
    
    # If there are no points left, or if on the last point for the second time
    # break while loop
    if ((num_pts_end_loop == 1) && (num_pts_start_loop == 1)) {
      break
    }
  }
  
  close(pb)
  
  # Print resolution fallback message if used
  if (resolution_fallback_used) {
    message(paste0("Note: The requested resolution '", resolution, 
                   "' was unavailable at some points. Data was pulled using '", 
                   resolution_used, "' instead."))
  }
  
  sf::sf_use_s2(TRUE)
  
  if (crop == TRUE) {
    # Crop if arg is TRUE
    final_geo <- geo_1 %>%
      sf::st_crop(aoi %>% sf::st_transform(sf::st_crs(geo_1)))
  } else {
    final_geo <- geo_1
  }
  
  return(final_geo)
}
