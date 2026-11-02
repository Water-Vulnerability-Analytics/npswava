#' @title Utah water rights points of diversion in an area
#'
#' @description
#' Downloads the Utah Division of Water Rights points of diversion within
#' the bounding box of `aoi` (plus `dist`), from the Utah Geospatial Resource
#' Center feature service
#' (<https://hub.arcgis.com/datasets/utahDNR::utah-points-of-diversion/about>),
#' so it needs internet. Because the query uses the bounding box, points
#' outside `aoi` but inside its box are returned too; `within_aoi` tells them
#' apart.
#'
#' Column descriptions are in the layer's metadata
#' (<https://www.arcgis.com/sharing/rest/content/items/5d530e62e6ca42528dd13e0a453a3b73/info/metadata/metadata.xml?format=default&output=html>).
#' The use codes are D = domestic, I = irrigation, M = municipal, O = other,
#' P = power, S = stock watering, X = mining.
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
#'   diversion, with the service's columns plus `UNIT_CODE` (`NA` outside
#'   `aoi`), `park_right` (`"Park"` when `OWNER` contains "National Park",
#'   otherwise `"Non-park"`) and `within_aoi` (`"Within AOI"` or `NA`).
#' @export
#' @seealso [getWaterSuppliersUtah()] and [getWaterUseUtah()] for Utah public
#'   water systems; the other state functions, e.g. [getPODColorado()].
#' @examplesIf interactive()
#' # Downloads the park boundaries and the points of diversion, so it needs
#' # internet
#' zion <- wavacore::getParkBoundary("ZION")
#' pods <- getPODUtah(zion, dist = 1000)
#' table(pods$within_aoi, useNA = "ifany")

getPODUtah <- function(aoi, dist = 0){
  
  # add buffer around park boundary
  aoi_buffer <- aoi %>% 
    sf::st_buffer(dist = dist)
  
  ## convert park boundary bbox to esri geom object
  filter_geom <- aoi_buffer %>%
    sf::st_transform(4326) %>%
    sf::st_bbox() %>% 
    sf::st_as_sfc(filter_geom) %>% 
    .[[1]] %>% 
    arcgisutils::as_esri_geometry()
 
  ## create the rest API
  # see: https://hub.arcgis.com/datasets/utahDNR::utah-points-of-diversion/about
  url <- paste0("https://services.arcgis.com/ZzrwjTRez6FJiOq4/arcgis/rest/services/Utah_Points_of_Diversion/FeatureServer/0",
                "/query?returnGeometry=true&where=1=1&outFields=*&f=geojson&geometry=",
                filter_geom,
                "&geometryType=esriGeometryPolygon&inSR=4326&spatialRel=esriSpatialRelIntersects")
  
  ut_wr <- sf::st_read(url) %>%
    sf::st_transform(sf::st_crs(aoi))
  
  #join sf object and table attributes and filter to park boundary region of interest
  nearby_wr <- ut_wr %>%    
    sf::st_join(.,dplyr::select(aoi, within = UNIT_CODE), left = TRUE) %>%
    dplyr::mutate(park_right = ifelse(grepl("National Park", OWNER, ignore.case = T), "Park","Non-park"),
                  within_aoi = ifelse(!is.na(within), "Within AOI", NA)) %>%
    dplyr::select(-within)
  
  return(nearby_wr)
  
}
