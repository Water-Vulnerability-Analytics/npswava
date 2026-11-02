#' @title SNOTEL and SCAN station data in an area
#'
#' @description
#' Finds the SNOTEL and SCAN stations inside `aoi` and its buffer (from the
#' station list in soilDB) and downloads their data with
#' [soilDB::fetchSCAN()], so it needs internet.
#'
#' It loads `SCAN_SNOTEL_metadata` into the global environment with
#' [utils::data()].
#'
#' @param aoi sf object. The area of interest, as polygons, e.g. a park
#'   boundary from [wavacore::getParkBoundary()]. Required.
#' @param dist Numeric. The distance to buffer `aoi` by, in meters for a
#'   longitude/latitude CRS (like the park boundaries) or in the CRS units
#'   otherwise.
#'   * `0`: only stations inside `aoi`.
#'   * A positive distance: also stations within that distance of it.
#'
#'   Default: `0`.
#' @param years `"all"` or a numeric vector of years. The years to download.
#'   * `"all"`: from the earliest station start year to this year.
#'   * Years, e.g. `2010:2020`: from the smallest to the largest of them
#'     (gaps in the vector are filled in).
#'
#'   Default: `"all"`.
#' @param timeseries String. `"Daily"` or `"Hourly"`, passed to
#'   [soilDB::fetchSCAN()]. Default: `"Daily"`.
#' @param save Logical. Not used yet: nothing is written. Default: `FALSE`.
#' @param path String or `NULL`. Not used yet. Default: `NULL`.
#'
#' @return A data frame (not sf), one row per station, sensor and time step,
#'   with every column as character, joined to the station metadata by
#'   `Site`. If no station is in the area, prints "No sites found." and
#'   returns `NULL`.
#' @export
#' @seealso [getNOAA()] for GHCN-Daily stations.
#' @examplesIf interactive()
#' # Downloads the park boundaries and the station data, so it needs internet
#' romo <- wavacore::getParkBoundary("ROMO")
#' snotel <- getSNOTEL(romo, years = 2020)
#' head(snotel)

getSNOTEL <- function(aoi, 
                      dist = 0, 
                      years = "all", 
                      timeseries = "Daily", 
                      save = FALSE, 
                      path = NULL) {
    
    # 1. Buffer and Transform AOI 
    aoi_buf <- sf::st_buffer(aoi, dist = dist)
    aoi_wgs84 <- sf::st_transform(aoi_buf, 4326)
    
    # 2. Filter sites quickly using WGS84
    utils::data('SCAN_SNOTEL_metadata', package = 'soilDB')
    sites <- sf::st_as_sf(SCAN_SNOTEL_metadata, coords = c("Longitude", "Latitude"), crs = 4326)
    
    site_aoi <- sites[aoi_wgs84, ] # Spatial Subset
    
    if (nrow(site_aoi) == 0) return(message("No sites found."))
    
    # 3. Determine Year Range
    # Use years[1] to check the first element of the input
    if (years[1] == "all") {
      start_yr <- min(lubridate::year(site_aoi$beginDate), na.rm = TRUE)
      end_yr   <- lubridate::year(Sys.Date())
    } else {
      # If it's a vector like 2010:2012, we just take the min and max
      start_yr <- min(as.numeric(years))
      end_yr   <- max(as.numeric(years))
    }
    
    # 4. BATCH REQUEST 
    message(paste("Fetching data for", nrow(site_aoi), "sites in one go..."))
    
    # fetchSCAN returns a list of dataframes
    all_dat_list <- soilDB::fetchSCAN(
      site.code = site_aoi$Site, 
      year = start_yr:end_yr, 
      timeseries = timeseries
    )
    
    metadata <- all_dat_list[names(all_dat_list) == "metadata"]$metadata
    
    # 5. Clean up efficiently
    final_data <- all_dat_list[names(all_dat_list) != "metadata"] %>%
      purrr::map(~ dplyr::mutate(.x, dplyr::across(dplyr::everything(), as.character))) %>%
      dplyr::bind_rows() %>%
      dplyr::left_join(metadata, by = "Site")
    
    return(final_data)
}
