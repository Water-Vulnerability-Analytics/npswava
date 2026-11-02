#' @title NOAA GHCN-Daily weather data for stations in an area
#'
#' @description
#' Finds the GHCN-Daily weather stations inside `aoi` ([rnoaa::ghcnd_stations()])
#' and downloads their daily data with [rnoaa::meteo_pull_monitors()], so it
#' needs internet and the rnoaa package (in Suggests, installed from GitHub).
#' Depending on the dates and the number of stations this can take a while.
#'
#' @param aoi sf object. The area of interest, as polygons, e.g. a park
#'   boundary from [wavacore::getParkBoundary()]. Required.
#' @param park String. The park code, used only to name the file when
#'   `save = TRUE` (`<park>_noaa.shp`). Required when `save = TRUE`.
#' @param startDate String. The first date to download.
#'   * `"first"`: January 1 of the earliest year any of the stations starts.
#'   * A date, `"YYYY-MM-DD"`.
#'
#'   Default: `"first"`.
#' @param endDate Date or string (`"YYYY-MM-DD"`). The last date to download.
#'   Default: `Sys.Date()`.
#' @param variables Character vector. The GHCN-Daily variables to download,
#'   passed to `var` of [rnoaa::meteo_pull_monitors()], e.g.
#'   `c("PRCP", "TMAX", "TMIN")`, or `"all"` for every variable. Default:
#'   `"all"`.
#' @param save Logical. Whether to write the result to a shapefile.
#'   * `TRUE`: writes `<path>/<park>_noaa.shp`, replacing an existing file.
#'   * `FALSE`: writes nothing.
#'
#'   Default: `FALSE`.
#' @param path String or `NULL`. The folder for the shapefile when
#'   `save = TRUE`; ignored otherwise. Default: `NULL`.
#'
#' @return An sf object of station points, one row per station and day, with
#'   `id`, `date`, one column per variable (in GHCN-Daily units), `elevation`
#'   and `name`.
#' @export
#' @seealso [getSNOTEL()] for SNOTEL and SCAN stations.
#' @examplesIf interactive() && requireNamespace("rnoaa", quietly = TRUE)
#' # Downloads the station list and the daily data, so it needs internet
#' zion <- wavacore::getParkBoundary("ZION")
#' weather <- getNOAA(zion, park = "ZION", startDate = "2020-01-01",
#'                    endDate = "2020-12-31", variables = c("PRCP", "TMAX"))
#' head(weather)

getNOAA <-
  function(aoi,
           park,
           startDate = "first",
           endDate = Sys.Date(),
           #noaa_key,
           variables = "all",
           save = FALSE,
           path = NULL) {
    
    # if (is.null(noaa_key)) {
    #   print("Must supply your NOAA key")
    #   stop()
    # }
    
    
    #options(noaakey = noaa_key)
    
    
    #pull all stations and make spatial
    all <- rnoaa::ghcnd_stations() %>%
      sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
    
    
    #filter to aoi
    station_aoi <- all %>%
      sf::st_transform(sf::st_crs(aoi)) %>%
      sf::st_filter(aoi) %>%
      dplyr::group_by(id, elevation, name, geometry, first_year) %>%
      dplyr::summarize() %>%
      sf::st_join(aoi) %>%
      dplyr::distinct(.keep_all = TRUE)
    
    
    # if startDate = 'first', find min year in the station metadata
    if (startDate == 'first'){
      startDate <- paste(min(station_aoi$first_year), "01", "01", sep = "-")
    }
    
    station_aoi <- station_aoi %>%
      dplyr::group_by(id, elevation, name, geometry) %>%
      dplyr::summarize() %>%
      dplyr::distinct(.keep_all = TRUE)
    
    #get weather data for all stations (depending on dates and how many stations this can take a while)
    weather_data <- rnoaa::meteo_pull_monitors(station_aoi$id, date_min = startDate,
                                               date_max = endDate,
                                               var = variables)
    
    
    #tie back to coordinates
    final_data <- weather_data %>%
      dplyr::left_join(., station_aoi, by = "id") %>%
      sf::st_as_sf() #add argument on whether to return sf or tibble?
    
    if (save == TRUE) {
      
        # save park boundary in park folder
        sf::st_write(final_data,
                     paste0(path, "/", park, "_noaa.shp"),
                     append = FALSE
                     )

      print(paste("NOAA data for specified dates saved to respective NPS park folder in:", paste0(getwd(), "/", path)))
      
      }
      
    
    return(final_data)
    
  }
