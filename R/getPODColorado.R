#' @title Colorado points of diversion and water rights in an area
#'
#' @description
#' Downloads every Colorado structure (structures used to divert, release,
#' store and manage water) from the Colorado Division of Water Resources
#' (<https://dnrftp.state.co.us/CDSS/GIS/Structure.zip>), keeps the active
#' ones (`CurrInUse` of U, A or F) inside `aoi` and its buffer, and adds each
#' structure's details and net water rights from the CDSS REST API, one
#' request per structure. It needs internet.
#'
#' If the API requests fail, the structures are returned without the API
#' columns. Column descriptions are at
#' <https://dwr.state.co.us/Tools/Structures/DataDictionary/GetDataDictionary>
#' and <https://dwr.state.co.us/Rest/GET/Help/Api/GET-api-v2-waterrights-netamount>.
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
#' @return An sf object of points (CRS NAD83), one row per structure and
#'   water right, with the structure columns, `UNIT_CODE`, `within_park`
#'   (`"Within Park"` or `NA` in the buffer), the CDSS structure and water
#'   rights columns, and `park_right` (`"Park"` when the contacts contain
#'   "National Park", otherwise `"Non-park"`). With no structures in the
#'   area, it prints a message and returns the empty sf object.
#' @export
#' @seealso The other state functions, e.g. [getPODUtah()] and
#'   [getWaterRightsMontana()].
#' @examplesIf interactive()
#' # Downloads the statewide structures file and queries the CDSS API, so it
#' # needs internet and takes a few minutes
#' colm <- wavacore::getParkBoundary("COLM")
#' structures <- getPODColorado(colm)
#' head(structures)

getPODColorado <- function(aoi, dist = 0){
  
  call <- "https://dnrftp.state.co.us/CDSS/GIS/Structure.zip"  #download boundary
  temp1 <- tempfile()
  utils::download.file(paste0(call), destfile = temp1, method = "curl")
  
  temp2 <- tempfile()
  utils::unzip(temp1, exdir = temp2)
  
  # add buffer around park boundary
  aoi_buffer <- aoi %>%
    sf::st_zm() %>%
    sf::st_buffer(dist = dist)
  
  co_wr <- sf::st_read(dsn = temp2) %>%
    sf::st_make_valid() %>% 
    sf::st_transform(4269)
  
  #join sf object and table attributes and filter to park boundary region of interest
  nearby_wr <- co_wr %>%
    #dplyr::filter(FeatureTyp == "Point of Diversion") %>%
    sf::st_intersection(., dplyr::select(aoi_buffer, UNIT_CODE)) %>%
    sf::st_join(.,dplyr::select(aoi, within = UNIT_CODE), left = TRUE) %>%
    dplyr::mutate(#park_right = ifelse(grepl("National Park", OWNER, ignore.case = T), "Park","Non-park"),
      within_park = ifelse(!is.na(within), "Within Park", NA)) %>%
    dplyr::select(-within) %>%
    dplyr::filter(stringr::str_detect(CurrInUse, "(U)|(A)|(F)")) # Filter for active structures only
  
  if(nrow(nearby_wr) == 0){
    print("No PODs found within the user-supplied `aoi`")
    
    return(nearby_wr)}
  
  div_downloader <- function(WDID){
    
    call <- paste0("https://dwr.state.co.us/Rest/GET/api/v2/structures/?wdid=", WDID)
    
    dat <- httr::GET(call)
    
    # convert content to text
    dat_text <- httr::content(dat, "text", encoding = "UTF-8")
    
    if(dat_text == "This URL is properly formatted, but returns zero records from CDSS."){final_df <- NULL} else {
      #parse data in JSON
      dat_json <- jsonlite::fromJSON(dat_text, flatten = TRUE)
      
      final_df <- dplyr::as_tibble(dat_json)
    }
    
    return(final_df)
  }
  
  try(divs_api <-  nearby_wr$WDID %>%
        purrr::map(~ try(div_downloader(.), silent = TRUE)) %>%
        dplyr::bind_rows() %>%
        .$ResultList %>%
        dplyr::distinct(.keep_all = TRUE), silent = TRUE)
  
  wr_downloader <- function(WDID){
    
    call <- paste0("https://dwr.state.co.us/Rest/GET/api/v2/waterrights/netamount/?format=json&wdid=", WDID)
    
    dat <- httr::GET(call)
    
    # convert content to text
    dat_text <- httr::content(dat, "text", encoding = "UTF-8")
    
    if(dat_text == "This URL is properly formatted, but returns zero records from CDSS."){final_df <- NULL} else {
      #parse data in JSON
      dat_json <- jsonlite::fromJSON(dat_text, flatten = TRUE)
      
      final_df <- dplyr::as_tibble(dat_json)
    }
    
    return(final_df)
  }
  
  try(wr_api <-  nearby_wr$WDID %>%
        purrr::map(~ try(wr_downloader(.), silent = TRUE)) %>%
        dplyr::bind_rows() %>%
        .$ResultList %>%
        dplyr::distinct(.keep_all = TRUE), silent = TRUE)
  
  try(final_wr <- dplyr::left_join(nearby_wr, divs_api, by = c("WDID"= "wdid")) %>%
        dplyr::left_join(., wr_api,by = c("WDID"= "wdid")) %>%
        dplyr::rename(associatedCaseNumbers = associatedCaseNumbers.x,
               waterDistrict = waterDistrict.x, 
               structureType = structureType.x, 
               gnisId = gnisId.x, 
               division = division.x, 
               locationAccuracy = locationAccuracy.x, 
               streamMile = streamMile.x, 
               structureName = structureName.x) %>%
        # duplicate info columns
        dplyr::select(-c(county.x, county.y, gnisId.y, division.y, latitude, longitude, locationAccuracy.y, pm.x, pm.y, 
                  range.y, section.y, streamMile.y, structureName.y, structureType.y, township.y, waterDistrict.y,
                  waterSource.x, waterSource.y, associatedCaseNumbers.y)) %>%
        dplyr::mutate(park_right = ifelse(grepl("National Park", associatedContacts, ignore.case = T), "Park","Non-park"))
  , silent = TRUE)
  
  if (exists("final_wr")) {
    return_dat <- final_wr
  } else(
    return_dat <- nearby_wr
  )

return(return_dat)

}
