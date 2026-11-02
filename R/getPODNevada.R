#' @title Nevada water rights points of diversion in an area
#'
#' @description
#' Reads the Nevada points of diversion from two local files in the shared
#' data folder, `all/POD_Permits_All.shp` and
#' `all/Water_Rights_Points_of_Diversion.csv`, joins them by application
#' number (`app`), and keeps the points inside `aoi` and its buffer. It
#' stops with an error if either file is missing.
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
#' @return An sf object of points (CRS NAD83), one row per point of diversion
#'   in the area, with the shapefile and table columns, the `aoi` columns and
#'   `PARK_RIGHT` (`"Park"` when `owner_name` contains "National Park",
#'   otherwise `"Non-park"`).
#' @export
#' @seealso The other state functions, e.g. [getPODUtah()].
#' @examplesIf file.exists(wavacore::wava_data_path("all", "POD_Permits_All.shp")) && interactive()
#' grba <- wavacore::getParkBoundary("GRBA")
#' pods <- getPODNevada(grba, dist = 1000)
#' table(pods$PARK_RIGHT)

getPODNevada <- function(aoi, dist=0){
  
  # add buffer around park boundary  
  aoi_buffer <- aoi %>%
    sf::st_buffer(dist = dist) 
  
  #read in POD attribute table
  table <- readr::read_csv(wavacore::wava_data_path("all", "Water_Rights_Points_of_Diversion.csv"))
  
  #read in spatial points of diversion
  nv_wr <- sf::st_read(wavacore::wava_data_path("all", "POD_Permits_All.shp")) %>%
    sf::st_drop_geometry() %>%
    dplyr::inner_join(.,table, by="app") %>%
    sf::st_as_sf(.,coords=c("longitude","latitude"), crs = 4269) %>%
    sf::st_transform(crs=4269) %>%
    sf::st_intersection(., aoi_buffer) %>%
    #sf::st_join(.,dplyr::select(aoi,within=UNIT_CODE),left=TRUE) %>%
    dplyr::mutate(PARK_RIGHT=ifelse(grepl("National Park", owner_name, ignore.case=T), "Park", "Non-park"))
    #within_park=ifelse(UNIT_CODE==within,"Within","Outside")) %>%
    #select(-within)
  
  return(nv_wr)
  
}


