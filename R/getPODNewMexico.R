#' @title New Mexico water rights points of diversion in an area
#'
#' @description
#' Downloads the New Mexico Office of the State Engineer points of diversion
#' (<https://geospatialdata-ose.opendata.arcgis.com/datasets/OSE::ose-pods/about>)
#' within the bounding box of `aoi` (plus `dist`), so it needs internet, and
#' keeps those inside `aoi` and its buffer.
#'
#' @param aoi sf object. The area of interest, as polygons with a
#'   `UNIT_CODE` column, e.g. a park boundary from
#'   [wavacore::getParkBoundary()]. Required.
#' @param dist Numeric. The distance to buffer `aoi` by before selecting
#'   features, in meters for a longitude/latitude CRS (like the park
#'   boundaries) or in the CRS units otherwise.
#'   * `0`: only features inside `aoi`.
#'   * A positive distance: also features within that distance of it.
#'
#'   Default: `0`.
#'
#' @return An sf object of points (in the CRS of `aoi`), one row per point of
#'   diversion, with the service's columns, the `aoi` columns, `park_right`
#'   (`"Park"` when `own_lname` contains "National Park", otherwise
#'   `"Non-park"`; this doesn't catch every park right) and `within_aoi`
#'   (`"Within AOI"` or `"Within buffer"`).
#' @export
#' @seealso The other state functions, e.g. [getPODUtah()] and
#'   [getPODColorado()].
#' @examplesIf interactive()
#' # Downloads the park boundaries and the points of diversion, so it needs
#' # internet
#' band <- wavacore::getParkBoundary("BAND")
#' pods <- getPODNewMexico(band, dist = 1000)
#' table(pods$within_aoi, pods$park_right)

getPODNewMexico <- function(aoi, dist = 0){
  
  # add buffer around park boundary
  aoi_buffer <- aoi %>% 
    sf::st_buffer(dist = dist)
  
  if(sf::st_crs(aoi_buffer)$epsg != 4326){
  
  ## convert park boundary bbox to esri geom object
  filter_geom <- aoi_buffer %>%
    sf::st_transform(4326) %>%
    sf::st_bbox() %>% 
    sf::st_as_sfc(filter_geom) %>% 
    .[[1]] %>% 
    arcgisutils::as_esri_geometry()
 
   } else {
    
     ## convert park boundary bbox to esri geom object
     filter_geom <- aoi_buffer %>%
       sf::st_bbox() %>% 
       sf::st_as_sfc(filter_geom) %>% 
       .[[1]] %>% 
       arcgisutils::as_esri_geometry()
    
  }
  
  ## create the rest API
  url <- paste0("https://services2.arcgis.com/qXZbWTdPDbTjl7Dy/arcgis/rest/services/OSE_PODs/FeatureServer/0/query?returnGeometry=true&where=1=1&outFields=*&f=geojson&geometry=",
                filter_geom,
                "&geometryType=esriGeometryPolygon&inSR=4326&spatialRel=esriSpatialRelIntersects")
  
  if(sf::st_crs(aoi_buffer)$epsg != 4326){
  
  nm_wr <- sf::st_read(url) %>%
    sf::st_transform(sf::st_crs(aoi))
  
  } else {
    
    nm_wr <- sf::st_read(url) 
    
  }
  
  # join sf object and table attributes and filter to park boundary region of interest
  nearby_wr <- nm_wr %>%  
    # subset to only PODs within aoi + buffer
    sf::st_intersection(., aoi_buffer) %>%
    # identify those found within aoi OR just within the buffer
    sf::st_join(., aoi %>% dplyr::mutate(within_aoi = "Within AOI") %>% dplyr::select(within_aoi), left = TRUE) %>%
    # identify PODs owned by park. This doesn't always work. 
    dplyr::mutate(park_right = ifelse(grepl("National Park", own_lname, ignore.case = T), "Park", "Non-park"),
                  within_aoi = ifelse(!is.na(within_aoi), "Within AOI", "Within buffer")) %>%
    sf::st_make_valid()
  
  return(nearby_wr)
 
}
