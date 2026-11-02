#' @title Geologic map units at points (Macrostrat)
#'
#' @description
#' Gets the geologic map units at each point from the Macrostrat API
#' (<https://macrostrat.org/>), including the units of every map source that
#' covers the point (from global to local scale), so it needs internet. One
#' request is made per distinct point.
#'
#' @param points sf object. The points, in any CRS (they are converted to
#'   longitude/latitude). Repeated coordinates are requested once. Required.
#'
#' @return An sf object of geologic unit polygons (CRS 4326) with the
#'   Macrostrat columns (e.g. `map_id`, `source_id`, `name`, `strat_name`,
#'   `lith`, `b_int_age`, `t_int_age`, `b_age`, `t_age`) and a column named
#'   `aoi$rowid[i]` holding the number of the point (among the distinct
#'   points) each unit came from. Points with no units add no rows.
#' @export
#' @seealso [get_rockd_aoi()] for an area; [get_formation_info()] to look up
#'   a formation by name; [get_surficial_geology()] for USGS surficial
#'   geology.
#' @examplesIf interactive()
#' # Queries the Macrostrat API, so it needs internet
#' pt <- sf::st_as_sf(data.frame(lon = -112.95, lat = 37.25),
#'                    coords = c("lon", "lat"), crs = 4326)
#' units <- get_rockd(pt)
#' unique(units$name)

get_rockd <- function(points){

  #https://macrostrat.org/api/v2/geologic_units/map?lat=38.7806&lng=-109.5936&adjacents=true&format=geojson_bare
  # Transform points to 4326 if not already 4326, then 
  # grab the coordinates for later steps:
  if(sf::st_crs(points)$epsg != 4326) {
    aoi <- points %>%
      sf::st_transform(4326) %>%
      sf::st_coordinates() %>%
      dplyr::as_tibble() %>%
      dplyr::distinct(X, Y) %>%
      tibble::rowid_to_column()
  
    } else {
      # Grab the coordinates for later steps:
    aoi <- points %>%
      sf::st_coordinates() %>%
      dplyr::as_tibble() %>%
      dplyr::distinct(X, Y) %>%
      tibble::rowid_to_column()
  }
  
  sf_temp <- list()
  
  for(i in 1:nrow(aoi)) {
    # grab geologic layers that intersect the coordinates of the points of interest
    y <- RCurl::getURL(paste0("https://macrostrat.org/api/v2/geologic_units/map/?lat=",aoi$Y[i],
                              "&lng=",
                              aoi$X[i],
                              "&adjacents=true&format=geojson_bare"),#geologic_units/map?strat_name_id=2683&format=geojson_bare",
                       .opts = RCurl::curlOptions(ssl.verifypeer=FALSE, verbose=FALSE)) %>%
      stringr::str_replace_all(., "\u000B", "")
    
    # If not an empty object, then extract geojson
    if(jsonlite::fromJSON(y)$features %>% length() > 0) {
      sf_temp[[i]] <- geojsonsf::geojson_wkt(y) %>%
        dplyr::mutate(aoi$rowid[i])
    }
  }
  
  sf_temp <- sf_temp %>% dplyr::bind_rows() %>%
    sf::st_as_sf(., wkt = 'geometry', crs = 4326)
  
  return(sf_temp)
  
}

