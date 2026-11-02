#' @title Drainage basin of a USGS gage
#'
#' @description
#' Gets the drainage basin upstream of a USGS stream gage from the NLDI
#' (Network Linked Data Index) with [nhdplusTools::get_nldi_basin()], so it
#' needs internet.
#'
#' @param site_no String. One USGS site number without the `"USGS-"` prefix,
#'   e.g. `"09405500"`. Required.
#'
#' @return An sf polygon (CRS NAD83) of the basin with a `site_no` column,
#'   or `NA` if the request fails (e.g. the site isn't in the NLDI).
#' @export
#' @seealso [listNWIS()] to find gages; [getXYWatersheds()] for the
#'   watershed of any point.
#' @examplesIf interactive()
#' # Queries the NLDI, so it needs internet
#' basin <- nldi_finder("09405500")
#' sf::st_area(basin)

nldi_finder <- function(site_no) {
  #get those gages' watersheds using get_nldi_basin in the {nhdplusTools}.
  nldi_nwis <- list(featureSource = "nwissite",
                    featureID = paste0("USGS-", site_no))
  
  gage_basin <- 
  tryCatch({
    invisible(
      suppressMessages(
          nhdplusTools::get_nldi_basin(nldi_feature = nldi_nwis) %>%
          sf::st_transform(., 4269) %>%
          dplyr::mutate(site_no = site_no)))
    }, error = function(e) {
      NA
    })
  
  return(gage_basin)
}
