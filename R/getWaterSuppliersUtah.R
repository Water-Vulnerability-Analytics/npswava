#' @title Utah culinary water service areas in an area
#'
#' @description
#' Downloads the Utah Division of Drinking Water culinary water service area
#' boundaries from the Utah Geospatial Resource Center, so it needs internet,
#' and keeps the parts inside `aoi` and its buffer.
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
#' @return An sf object of polygons (in the CRS of `aoi`), one row per
#'   service area and park, with the service area columns, `UNIT_CODE`,
#'   `park_right` (`"Park"` when `WRNAME` contains "National Park",
#'   otherwise `"Non-park"`) and `within_park` (`"Within Park"` or `NA` in the
#'   buffer).
#' @export
#' @seealso [getWaterUseUtah()] for the water use of a public water system;
#'   [getPODUtah()] for points of diversion.
#' @examplesIf interactive()
#' # Downloads the statewide service areas, so it needs internet
#' zion <- wavacore::getParkBoundary("ZION")
#' suppliers <- getWaterSuppliersUtah(zion, dist = 5000)
#' suppliers

getWaterSuppliersUtah <- function(aoi, dist = 0){
  
  call <- "https://opendata.arcgis.com/api/v3/datasets/dc62a286013f447e88fc45480077c944_0/downloads/data?format=shp&spatialRefId=26912&where=1%3D1"
  #download boundary 
  temp1 <- tempfile()
  utils::download.file(paste0(call), destfile = temp1, method = "curl")
  
  temp2 <- tempfile()
  utils::unzip(temp1, exdir = temp2)
  
  # add buffer around park boundary
  aoi_buffer <- aoi %>% 
    sf::st_buffer(dist = dist)
  
  ut_wu <- sf::st_read(dsn = temp2) %>%
    sf::st_make_valid() %>% 
    sf::st_transform(sf::st_crs(aoi))
  
  #join sf object and table attributes and filter to park boundary region of interest
  nearby_wu <- ut_wu %>%    
    sf::st_intersection(dplyr::select(aoi_buffer, UNIT_CODE)) %>%
    sf::st_join(.,dplyr::select(aoi, within = UNIT_CODE), left = TRUE) %>%
    dplyr::mutate(park_right = ifelse(grepl("National Park", WRNAME, ignore.case = T), "Park","Non-park"),
                  within_park = ifelse(!is.na(within), "Within Park", NA)) %>%
    dplyr::select(-within)
  
  return(nearby_wu)
  
}


