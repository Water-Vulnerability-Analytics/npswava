#' @title Montana water rights in an area
#'
#' @description
#' Downloads the Montana Department of Natural Resources and Conservation
#' water rights geodatabase
#' (<https://ftpgeoinfo.msl.mt.gov/Data/Spatial/NonMSDI/DNRC_WR/MTWaterRights.gdb.zip>),
#' so it needs internet, and keeps the features inside `aoi` and its buffer.
#' It reads the current operating authority layers: points of diversion
#' (`WRDIV`), places of use (`WRPOU`) and reservoirs (`WRRES`). The layers
#' for original versions (`WR1*`) are not read.
#'
#' `WRKEY`, the unique ID of a water right, is the water right ID (`WRID`)
#' and its version (`VERID`) joined by a hyphen. The data are updated about
#' twice a year. Metadata:
#' <http://ftpgeoinfo.msl.mt.gov/Data/Spatial/NonMSDI/DNRC_WR/MTWaterRights_Metadata.xml>.
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
#' @return A list of three sf objects in the CRS of the geodatabase:
#'   `points_of_diversion`, `places_of_use` and `reservoirs`. Each has the
#'   layer's columns plus `UNIT_CODE`, `park_right` (`"Park"` when
#'   `ALLOWNERS` contains "National Park", otherwise `"Non-park"`) and
#'   `within_park` (`"Within Park"` or `NA` in the buffer).
#' @export
#' @seealso The other state functions, e.g. [getPODColorado()].
#' @examplesIf interactive()
#' # Downloads the statewide geodatabase, so it needs internet and takes a
#' # few minutes
#' biho <- wavacore::getParkBoundary("BIHO")
#' mt <- getWaterRightsMontana(biho, dist = 5000)
#' lapply(mt, nrow)

getWaterRightsMontana <- function(aoi, dist = 0){
  
  call <- "https://ftpgeoinfo.msl.mt.gov/Data/Spatial/NonMSDI/DNRC_WR/MTWaterRights.gdb.zip"
  temp1 <- tempfile()
  utils::download.file(paste0(call), destfile = temp1, method = "curl")
  temp2 <- tempfile()
  utils::unzip(temp1, exdir = temp2)
 
  POD <- sf::st_read(list.files(temp2, full.names = TRUE), layer = "WRDIV")
  POU <- sf::st_read(list.files(temp2, full.names = TRUE), layer = "WRPOU")
  RES <- sf::st_read(list.files(temp2, full.names = TRUE), layer = "WRRES")
  
  # These are layers containing the historic data (i.e., if any rights were
  # moved around or modififed, this layer shows the original owner, etc.)
  # POU1 <- st_read(list.files(temp2, full.names = TRUE), layer = "WR1POU")
  # POD1 <- st_read(list.files(temp2, full.names = TRUE), layer = "WR1DIV")
  # RES1 <- st_read(list.files(temp2, full.names = TRUE), layer = "WR1RES")
  

  aoi <- aoi  %>%
    sf::st_transform(terra::crs(POD))
  
  # add buffer around park boundary
  aoi_buffer <- aoi %>% 
    sf::st_zm() %>%
    sf::st_buffer(dist = dist)
  
  #join sf object and table attributes and filter to park boundary region of interest
  nearby_pods <- POD %>%    
    sf::st_intersection(., dplyr::select(aoi_buffer, UNIT_CODE)) %>%
    sf::st_join(.,dplyr::select(aoi, within = UNIT_CODE), left = TRUE) %>%
    dplyr::mutate(park_right = ifelse(grepl("National Park", ALLOWNERS, ignore.case = T), "Park","Non-park"),
                  within_park = ifelse(!is.na(within), "Within Park", NA)) %>%
    dplyr::select(-within)
  
  nearby_pous <- POU %>%
    sf::st_intersection(., dplyr::select(aoi_buffer, UNIT_CODE)) %>%
    sf::st_join(.,dplyr::select(aoi, within = UNIT_CODE), left = TRUE) %>%
    dplyr::mutate(park_right = ifelse(grepl("National Park", ALLOWNERS, ignore.case = T), "Park","Non-park"),
                  within_park = ifelse(!is.na(within), "Within Park", NA)) %>%
    dplyr::select(-within)

  nearby_res <- RES %>%
    sf::st_intersection(., dplyr::select(aoi_buffer, UNIT_CODE)) %>%
    sf::st_join(.,dplyr::select(aoi, within = UNIT_CODE), left = TRUE) %>%
    dplyr::mutate(park_right = ifelse(grepl("National Park", ALLOWNERS, ignore.case = T), "Park","Non-park"),
                  within_park = ifelse(!is.na(within), "Within Park", NA)) %>%
    dplyr::select(-within)
  
  # nearby_pod1 <- POD1 %>%
  #   sf::st_intersection(., dplyr::select(aoi_buffer, UNIT_CODE)) %>%
  #   sf::st_join(.,dplyr::select(aoi, within = UNIT_CODE), left = TRUE) %>%
  #   dplyr::mutate(park_right = ifelse(grepl("National Park", ALLOWNERS, ignore.case = T), "Park","Non-park"),
  #                 within_park = ifelse(!is.na(within), "Within Park", NA)) %>%
  #   dplyr::select(-within)
  
  # nearby_pou1 <- POU1 %>%
  #   sf::st_intersection(., dplyr::select(aoi_buffer, UNIT_CODE)) %>%
  #   sf::st_join(.,dplyr::select(aoi, within = UNIT_CODE), left = TRUE) %>%
  #   dplyr::mutate(park_right = ifelse(grepl("National Park", ALLOWNERS, ignore.case = T), "Park","Non-park"),
  #                 within_park = ifelse(!is.na(within), "Within Park", NA)) %>%
  #   dplyr::select(-within)
  
  # nearby_res1 <- RES %>%
  #   sf::st_intersection(., dplyr::select(aoi_buffer, UNIT_CODE)) %>%
  #   sf::st_join(.,dplyr::select(aoi, within = UNIT_CODE), left = TRUE) %>%
  #   dplyr::mutate(park_right = ifelse(grepl("National Park", ALLOWNERS, ignore.case = T), "Park","Non-park"),
  #                 within_park = ifelse(!is.na(within), "Within Park", NA)) %>%
  #   dplyr::select(-within)
  
  return(list("points_of_diversion" = nearby_pods, 
              "places_of_use" = nearby_pous,
              "reservoirs" = nearby_res))
  
}
