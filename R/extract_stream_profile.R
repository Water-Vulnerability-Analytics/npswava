#' @title Longitudinal profile of a stream's main channel
#'
#' @description
#' Finds the main channel of a stream network with [extract_mainstem_path()],
#' samples points along it at a regular spacing, and works out each point's
#' distance from the headwater, elevation and slope.
#'
#' @details
#' Distance and slope are measured between sampled points. Distance starts at
#' 0 at the first sampled point, which is half of `sample_dist` below the
#' start of the line. If the first point is lower than the last, distances are
#' reversed, so 0 is always the high end.
#'
#' Slope is `dz / dx` to the next point downstream, so the last point has
#' `NA` slope.
#'
#' @param flowlines sf object. A LINESTRING or MULTILINESTRING stream network.
#'   Required.
#' @param dem SpatRaster. An elevation raster covering `flowlines`, in meters
#'   for `elevation_m` and the slopes to be right. Required.
#' @param sample_dist Number. The spacing between sampled points, in meters.
#'   Default: `10`.
#' @param min_reach_length Number. The shortest main channel allowed, in
#'   meters. A shorter one stops with an error. Default: `50`.
#'
#' @return A list with:
#'   * `nodes`: an sf object of the sampled points, with `cum_dist_m`,
#'     `elevation_m`, `node_id`, `dz`, `dx`, `slope_m_per_m` and `slope_pct`.
#'   * `mainstem_flowline`: an sfc with the main channel as one LINESTRING.
#'   * `profile_plot`: a ggplot of elevation against distance, colored by
#'     slope.
#' @references
#' Dijkstra, E. W. (1959). A note on two problems in connexion with graphs.
#' *Numerische Mathematik*, 1(1), 269-271. \doi{10.1007/BF01386390}
#'
#' Hack, J. T. (1957). Studies of longitudinal stream profiles in Virginia and
#' Maryland. *U.S. Geological Survey Professional Paper*, 294-B, 45-97.
#' @export
#' @seealso [extract_mainstem_path()] for how the channel is found, and
#'   [delineate_streamlines()] for the network.
#' @examples
#' dem <- terra::rast(xmin = 0, xmax = 1000, ymin = 0, ymax = 1000,
#'                    resolution = 10, crs = "EPSG:5070")
#' terra::values(dem) <- 2000 + terra::yFromCell(dem, seq_len(terra::ncell(dem))) / 10 +
#'   abs(terra::xFromCell(dem, seq_len(terra::ncell(dem))) - 500) / 20
#'
#' # A channel with two branches above a confluence
#' flowlines <- sf::st_sf(geometry = sf::st_sfc(
#'   sf::st_linestring(rbind(c(500, 100), c(500, 500))),
#'   sf::st_linestring(rbind(c(500, 500), c(200, 900))),
#'   sf::st_linestring(rbind(c(500, 500), c(800, 950))),
#'   crs = 5070
#' ))
#'
#' profile <- extract_stream_profile(flowlines, dem, sample_dist = 50)
#' head(profile$nodes)
#' profile$profile_plot

extract_stream_profile <- function(flowlines, dem, sample_dist = 10, min_reach_length = 50) {
  
  # 1. Extract the headwater-to-outlet mainstem via graph shortest path
  #    (see R/extract_mainstem_path.R). This replaces a naive
  #    st_combine()+st_line_merge()+pick-longest-piece approach, which only
  #    merges through degree-2 nodes and therefore fractures -- and picks the
  #    wrong "longest" piece -- at every confluence. See that file's roxygen
  #    @details for the full explanation. extract_mainstem_path() handles the
  #    flowlines/dem CRS match internally.
  mainstem_result <- extract_mainstem_path(flowlines, dem)
  mainstem_line <- mainstem_result$mainstem_line
  
  if (mainstem_result$length_m < min_reach_length) {
    stop("Extracted mainstem path is shorter than min_reach_length threshold (",
         round(mainstem_result$length_m, 1), " m < ", min_reach_length, " m).")
  }
  
  # 2. Sample nodes along the continuous mainstem LINESTRING
  pts <- sf::st_line_sample(mainstem_line, density = 1 / sample_dist) |>
    sf::st_cast("POINT") |>
    sf::st_as_sf()
  
  # 3. Calculate continuous cumulative distance (meters)
  coords <- sf::st_coordinates(pts)
  dist_intervals <- c(0, sqrt(diff(coords[, 1])^2 + diff(coords[, 2])^2))
  pts$cum_dist_m <- cumsum(dist_intervals)
  
  # 4. Extract DEM elevation at each node
  pts$elevation_m <- terra::extract(dem, terra::vect(pts))[[2]]
  
  # 5. Orient from Headwater (high elevation) to Outlet (low elevation)
  # If the line was digitized downstream-to-upstream, reverse distance so 0 = headwater
  if (pts$elevation_m[1] < pts$elevation_m[nrow(pts)]) {
    pts$cum_dist_m <- max(pts$cum_dist_m) - pts$cum_dist_m
    pts <- pts[order(pts$cum_dist_m), ]
  }
  
  # 6. Calculate Reach Slope (m/m and %) along mainstem
  nodes <- pts |>
    dplyr::mutate(
      node_id = dplyr::row_number(),
      dz = elevation_m - dplyr::lead(elevation_m),
      dx = dplyr::lead(cum_dist_m) - cum_dist_m,
      slope_m_per_m = dz / dx,
      slope_pct = slope_m_per_m * 100
    )
  
  # 7. Generate Clean Profile Plot
  p <- ggplot2::ggplot(nodes, ggplot2::aes(x = cum_dist_m, y = elevation_m)) +
    ggplot2::geom_line(color = "royalblue", linewidth = 1) +
    ggplot2::geom_point(ggplot2::aes(color = slope_pct), size = 1.8) +
    ggplot2::scale_color_viridis_c(option = "magma", name = "Slope (%)", na.value = "grey50") +
    ggplot2::labs(
      title = "Mainstem Longitudinal Profile",
      subtitle = paste0("Total Length: ", round(max(nodes$cum_dist_m), 1), " m"),
      x = "Cumulative Distance Downstream (m)",
      y = "Elevation (m)"
    ) +
    ggplot2::theme_minimal()
  
  return(list(nodes = nodes, mainstem_flowline = mainstem_line, profile_plot = p))
}
