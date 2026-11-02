#' @title NPS buildings in a park
#'
#' @description
#' Downloads the NPS Buildings layer (all parks) from the NPS open data
#' portal
#' (<https://public-nps.opendata.arcgis.com/datasets/nps::nps-buildings-web-mercator>),
#' so it needs internet, and keeps the buildings inside `park_boundary` and
#' its buffer.
#'
#' @param park_boundary sf object. The park boundary, as polygons with a
#'   `UNIT_CODE` column, e.g. from [wavacore::getParkBoundary()]. Required.
#' @param dist Numeric. The distance to buffer `park_boundary` by, in meters
#'   for a longitude/latitude CRS (like the park boundaries) or in the CRS
#'   units otherwise.
#'   * `0`: only buildings inside the boundary.
#'   * A positive distance: also buildings within that distance of it.
#'
#'   Default: `0`.
#'
#' @return An sf object of building polygons (in the CRS of `park_boundary`),
#'   with the layer's columns and `UNIT_CODE`.
#' @export
#' @seealso [wavacore::getParkBoundary()] for the boundary.
#' @examplesIf interactive()
#' # Downloads the park boundaries and every NPS building, so it needs
#' # internet and takes a few minutes
#' zion <- wavacore::getParkBoundary("ZION")
#' buildings <- getBuildings(zion)
#' nrow(buildings)

getBuildings <- function(park_boundary, dist = 0){

  #install.packages("arcgisbinding", repos="https://r.esri.com", type="win.binary")
  call <- "https://opendata.arcgis.com/api/v3/datasets/2bf22e15d269431e9e3380382c0a1546_2/downloads/data?format=shp&spatialRefId=3857&where=1%3D1"
 
  temp1 <- tempfile()
  utils::download.file(paste0(call), destfile = temp1, method = "curl")
  temp2 <- tempfile()
  utils::unzip(temp1, exdir = temp2)

  buffer <- park_boundary %>% 
    sf::st_buffer(dist = dist)
  
  buildings <- sf::st_read(temp2, layer = "NPS_-_Buildings_-_Web_Mercator") %>%
    sf::st_make_valid() %>% 
    sf::st_zm() 
  
  if(sf::st_crs(buildings) != sf::st_crs(buffer)){
    buildings <- buildings %>%
      sf::st_transform(sf::st_crs(park_boundary))
  }
  
  buildings <- buildings %>%
    sf::st_join(., dplyr::select(buffer, UNIT_CODE)) %>%
    dplyr::filter(!is.na(UNIT_CODE))

  return(buildings)
  
}

