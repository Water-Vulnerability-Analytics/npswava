#' @title Volume of water in a valley aquifer for a depth to water
#'
#' @description
#' Estimates how much water a valley (alluvial) aquifer holds when the water
#' table is `dtw_ft` below the surface, treating the aquifer as a triangular
#' prism. As the water table drops, the aquifer's sides (at angle
#' `theta_deg`) pull in, so the full extent is shrunk by
#' `(dtw_ft + valley_wall_elev) / tan(theta_deg)` feet. The volume is
#' `0.5 * area * (full_thickness_ft - valley_wall_elev - dtw_ft)`, times the
#' specific yield `sy`.
#'
#' Turns off s2 geometry with [sf::sf_use_s2()] while it runs and turns it
#' back on at the end (but not when `dtw_ft` is `NA`).
#'
#' @param dtw_ft Numeric. The depth to the water table, in feet. Negative
#'   values are made positive (depth is often recorded as negative). `NA`
#'   returns `NA`. One value; use [purrr::map_dbl()] for a series. Required.
#' @param aquifer_extent sf object. The aquifer's extent when full, as a
#'   polygon in a longitude/latitude CRS: the shrink distance is converted
#'   from feet to degrees (divided by 366,002.44). Required.
#' @param full_thickness_ft Numeric. The thickness of the deepest part of the
#'   aquifer, in feet. Required.
#' @param well_depth Numeric. The depth of the well, in feet.
#'   * At least `full_thickness_ft - valley_wall_elev`: the well reaches the
#'     bottom, and the whole saturated prism counts.
#'   * Less than that: the part below the well is meant to be subtracted, but
#'     this branch currently stops with an error (`buffer_length2` not
#'     found).
#'
#'   Required.
#' @param valley_wall_elev Numeric. The height, in feet, of the valley wall
#'   above the wells where depth to water is measured. Required.
#' @param theta_deg Numeric. The angle, in degrees, between the ground
#'   surface and the sides of the aquifer. Required.
#' @param sy Numeric from 0 to 1. The specific yield of the aquifer.
#'   Required.
#'
#' @return The volume of water in the aquifer, in cubic feet (a number), or
#'   `NA` when `dtw_ft` is `NA`.
#' @export
#' @seealso [add_gw_meta()] for well and aquifer information from NWIS.
#' @examples
#' # A made-up valley aquifer, about 2 km by 10 km
#' extent <- sf::st_as_sf(sf::st_sfc(
#'   sf::st_polygon(list(rbind(c(-112.20, 37.60), c(-112.18, 37.60),
#'                             c(-112.18, 37.69), c(-112.20, 37.69),
#'                             c(-112.20, 37.60)))),
#'   crs = 4326
#' ))
#'
#' aquifer_volume(dtw_ft = 15, aquifer_extent = extent, full_thickness_ft = 60,
#'                well_depth = 60, valley_wall_elev = 30, theta_deg = 10,
#'                sy = 0.2)

aquifer_volume <- function(dtw_ft,
                           aquifer_extent,
                           full_thickness_ft,
                           well_depth,
                           valley_wall_elev,
                           theta_deg,
                           sy) {
  
  # Test inputs
  #aquifer_extent <- marine
  #full_thickness_ft <- 60 # ft
  #theta_deg <- 10 # degrees
  #dtw_ft <- spei_historical_gw$static_ft[1] # gw depth
  #sy <- .2 # unitless
  # well_depth <- 30 #ft approx depth of screened interval
  #valley_wall_elev <- 30 #ft from DEM
  
  suppressMessages(
    suppressWarnings(sf::sf_use_s2(FALSE)
                     )
    )
  
  if(is.na(dtw_ft)) {
    return(NA)
  } else {
    
    dtw_ft <- abs(dtw_ft) # often dtw is negative by convention even though
                          # it should be positive. Check this if applied
                          # anywhere other than BRCA
    
    # The valley surface slopes down (perpendicular to the EC drainage). 
    # Here, I assign the elevation change between the alluvium margin and 
    # the alluvium valley center (measured from google earth). It was 20-30
    # feet in this case, but could be changed.
    #valley_wall_elev <- 30
    
    # Calculate buffer distance for aquifer surface -- as the groundwater table
    # deepens, the effective "area" of the aquifer shrinks. This uses the approx.
    # valley angle reported by Marine (1962) to calculate the effective decrease
    # in aquifer width for a change in groundwater depth.
    buffer_length <- (dtw_ft + valley_wall_elev)/tan(theta_deg*pi/180) / 366002.44 # in decimal degrees
    
    # Use that buffer distance to remove buffer distance from the original
    # aquifer extent.
    new_aquifer_extent <- 
      suppressMessages(
        suppressWarnings(
          sf::st_buffer(aquifer_extent, 
                    -buffer_length))) # ft to lat/long
    
    # Calculate the area of the new aquifer extent in ft2
    new_aquifer_area <- 
     sf::st_area(new_aquifer_extent) %>% 
      units::set_units("ft^2") %>%
      as.numeric()
    
    # Calculate the total "solid" volume of the new aquifer assuming a 
    # triangular prism.
    new_aquifer_volume <- 
      0.5 * new_aquifer_area * (full_thickness_ft - valley_wall_elev - dtw_ft)
    
    # IF well doesn't penetrate aquifer, calculate triangular prism from well depth
    if (well_depth < (full_thickness_ft - valley_wall_elev)) {
      buffer_length_2 <- (well_depth + valley_wall_elev)/tan(theta_deg*pi/180)
      new_aquifer_extent2 <-
        suppressMessages(
          suppressWarnings(
            sf::st_buffer(aquifer_extent,
                      buffer_length2/-366002.44)))
      new_aquifer_area2 <- as.numeric(sf::st_area(new_aquifer_extent2)) * 10.764
      unavailable_volume <-
        0.5 * new_aquifer_area * (full_thickness_ft - valley_wall_elev - well_depth)

      new_aquifer_volume <- new_aquifer_volume - unavailable_volume 
    }
    
    # Calculate the water volume in the aquifer assuming a given specific yield.
    water_volume <- 
      new_aquifer_volume * sy 
    
    
    suppressMessages(
      suppressWarnings(sf::sf_use_s2(TRUE)
      )
    )
    
    
    return(water_volume)
    
  }
}

#test <- well_data %>%
#  dplyr::ungroup() %>%
#  dplyr::filter(well == "Well 1") %>%
#  dplyr::mutate(aq_volume_ft3 = 
#                  map_dbl(static_ft_c, ~aquifer_volume(dtw_ft = .)))
