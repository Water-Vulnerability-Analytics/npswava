#' @title California water rights points of diversion in an area
#'
#' @description
#' Downloads the California eWRIMS points of diversion
#' (<https://waterrightsmaps.waterboards.ca.gov/viewer/index.html?viewer=eWRIMS.eWRIMS_gvh>)
#' by driving a headless Firefox with RSelenium, joins them to the point
#' attributes, and keeps those inside `aoi` and its buffer. It needs
#' internet, the RSelenium package (in Suggests), Java and Firefox.
#'
#' It creates a `temp_ewrims` folder in the working directory for the
#' download and deletes it at the end. It also sets
#' `options(timeout = 1000)` (or higher) for the session.
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
#'   and attribute record in the area, with `POD_ID`, `LATITUDE`,
#'   `LONGITUDE`, `DIVERSION_SITE_NAME`, `SOURCE_NAME`, the attribute columns,
#'   `UNIT_CODE`, `park_right` (`"Park"` when `LAST_NAME` contains "National
#'   Park", otherwise `"Non-park"`) and `within_park` (`"Within"` or
#'   `"Outside"`).
#' @export
#' @seealso The other state functions, e.g. [getPODUtah()].
#' @examples
#' \dontrun{
#' # Not run: needs RSelenium, Java and Firefox, and writes a temporary
#' # folder in the working directory
#' jotr <- wavacore::getParkBoundary("JOTR")
#' pods <- getPODCalifornia(jotr, dist = 1000)
#' }

getPODCalifornia <- function(aoi, dist=0){
  
  options(timeout = max(1000, getOption("timeout")))
  
  # create a temporary directory for all raw datafiles to be sent to
  dir.create(file.path(getwd(),"/temp_ewrims/"), showWarnings = FALSE)
  
  downloadPath <- file.path(getwd(),"/temp_ewrims/") #%>% stringr::str_replace_all("/", "\\\\\\\\")
  
  
  # make sure server downloads data into appropriate folder (i.e., our temporary folder)
  fprof <- RSelenium::makeFirefoxProfile(list(browser.download.dir = downloadPath,
                                              browser.download.folderList = 2L,
                                              browser.download.manager.showWhenStarting = FALSE,
                                              browser.helperApps.neverAsk.openFile = "text/csv/gdb/zip",
                                              browser.helperApps.neverAsk.saveToDisk = "text/csv/gdb/zip"))
  
  exCap <- list(firefox_profile = fprof$firefox_profile, 
                "moz:firefoxOptions" = list(args = list('--headless')))
  
  # start up RSelenium session:
  rD <- RSelenium::rsDriver(browser = "firefox", port = sample.int(1000, 1), extraCapabilities = exCap)
  remDr <- rD$client
  
  # get list of all Aquarius sites from the portal
  remDr$navigate('https://gispublic.waterboards.ca.gov/portal/home/item.html?id=746870b4aa1f47579ca1bf5d6c9924a8')
  Sys.sleep(7) # provide time for site navigation
  remDr$screenshot(display = TRUE)
  
  dropper <- remDr$findElement(using = 'xpath', value = '/html/body/div[2]/div/div[2]/div/div[2]/div/main/div[1]/aside/div/button[6]')
  Sys.sleep(3) # provide time for navigation and selection
  dropper$clickElement()
  Sys.sleep(15)
  # OLD? <- /html/body/div[3]/div/div[2]/div/div[2]/div/main/div[1]/aside/div/button[6]
  temp <- list.files(path = paste0(getwd(),"/temp_ewrims"), pattern = "*.zip", full.names = T)
  
  # unzip all your downloaded data files (one for every site)
  try(utils::unzip(temp,exdir=paste0(getwd(),"/temp_ewrims/eWRIMS.gdb")))
  
  # add buffer around park boundary  
  aoi_buffer <- aoi %>%
    sf::st_buffer(dist = dist) 
  
  #read in spatial points of diversion
  try(ca_wr <- sf::st_read(paste0(getwd(),"/temp_ewrims/eWRIMS.gdb")) %>%
    sf::st_transform(crs=4269))
  
  #read in POD attribute table
  try(table <- sf::st_read(paste0(getwd(),"/temp_ewrims/eWRIMS.gdb"), layer = "POD_Attributes"))
  
  #join sf object and table attributes and filter to park boundary region of interest
  nearby_wr <- ca_wr %>%
    dplyr::select(POD_ID,LATITUDE,LONGITUDE,DIVERSION_SITE_NAME,SOURCE_NAME) %>%
    dplyr::inner_join(., table, by=c("POD_ID"="CORE_POD_ID")) %>%
    sf::st_intersection(., dplyr::select(aoi_buffer, UNIT_CODE)) %>%
    sf::st_join(.,dplyr::select(aoi,within=UNIT_CODE),left=TRUE) %>%
    dplyr::mutate(park_right=ifelse(grepl("National Park", LAST_NAME, ignore.case=T), "Park","Non-park"),
                  within_park=ifelse(UNIT_CODE==within,"Within","Outside")) %>%
    dplyr::select(-within)
  
  # end the RSelenium server session
  remDr$close()
  rD[["server"]]$stop()
  
  # delete the temporary folder
  unlink(file.path(getwd(), "/temp_ewrims/"), recursive = T)
  return(nearby_wr)
  
}


