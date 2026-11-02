#' @title NHDPlus High Resolution flowlines in an area
#'
#' @description
#' Finds the HUC4 watersheds that overlap `aoi` with
#' [nhdplusTools::get_huc()] (so it needs internet), reads the `NHDFlowline`
#' layer of each one's NHDPlus HR geodatabase from the shared data folder
#' (`all/nhdplushr/<HU2>/<HU4>_HU4_GDB.gdb`), and keeps the flowlines that
#' intersect `aoi`.
#'
#' @param aoi sf object. The area of interest, as polygons, e.g. a park
#'   boundary from [wavacore::getParkBoundary()]. Required.
#'
#' @return An sf object of flowlines (CRS NAD83, Z and M dropped) with the
#'   NHDPlus HR flowline columns.
#' @export
#' @seealso [getXYWatersheds()] and [nldi_finder()] for watersheds.
#' @examplesIf interactive() && dir.exists(wavacore::wava_data_path("all", "nhdplushr"))
#' # Reads the NHDPlus HR geodatabase for each HUC4 (hundreds of MB each)
#' zion <- wavacore::getParkBoundary("ZION")
#' flowlines <- mapNHDPlusHR(zion)
#' nrow(flowlines)

mapNHDPlusHR <- function(aoi){
  
  hucs <- nhdplusTools::get_huc(AOI = aoi, type = "huc04")
  
  flowlines <- vector("list", length = nrow(hucs))
  
  for(i in 1:nrow(hucs)){
    
    nhdhr <- paste0(wavacore::wava_data_path("all", "nhdplushr"), "/", stringr::str_sub(hucs$huc4[i], 1, 2), "/")
    
    file <- list.files(nhdhr, pattern = paste0(hucs$huc4[i], "_HU4_GDB.gdb"), full.names = TRUE)
    
    # filter out and clip the catchment features within the park boundaries:
    flowlines[[i]] <- sf::st_read(dsn = file, layer = "NHDFlowline") %>%
      sf::st_zm()
    
    if(sf::st_crs(aoi)$epsg != 4269){
      flowlines[[i]] <- flowlines[[i]] %>%
        .[sf::st_transform(aoi, 4269),]
    } else {
      
      flowlines[[i]] <- flowlines[[i]] %>%
        .[aoi,]
    }
    
  }
  
  flowlines<- dplyr::bind_rows(flowlines) %>%
    
    
    return(flowlines)
  
}
