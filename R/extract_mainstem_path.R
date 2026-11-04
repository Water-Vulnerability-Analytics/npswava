#' @title Longest connected headwater-to-outlet path of a stream network
#'
#' @description
#' Builds a spatial network from a stream network, keeps its largest connected
#' part, finds the highest and lowest nodes in that part from a DEM (the
#' headwater and the outlet), and returns the shortest path between them by
#' edge length (Dijkstra's algorithm) as one continuous line.
#'
#' @details
#' A graph path is used instead of [sf::st_line_merge()] because
#' `st_line_merge()` only joins lines through nodes where exactly two lines
#' meet. At a confluence it can't tell which branch continues the line, so it
#' leaves the network split there. Picking the longest piece afterwards picks
#' the longest run between two confluences, often a short tributary or reach,
#' not the headwater-to-outlet channel. A graph path crosses every confluence,
#' however many branches meet there.
#'
#' This assumes `flowlines` share exact vertices at confluences, as lines from
#' [delineate_streamlines()] do. A network with small gaps at junctions (e.g.
#' some raw NHD products) may need [sf::st_snap()] or
#' `sfnetworks::to_spatial_subdivision()` first. A gap between edges of the
#' path larger than `snap_tol` gives a warning.
#'
#' @param flowlines sf object. A LINESTRING or MULTILINESTRING stream network,
#'   reprojected to the CRS of `dem` if needed. Required.
#' @param dem SpatRaster. An elevation raster covering `flowlines`, used to
#'   find the headwater (highest) and outlet (lowest) nodes. Required.
#' @param snap_tol Number. The largest gap allowed between consecutive edges
#'   of the path, in CRS units (typically meters), when they are joined. A
#'   larger gap gives a warning, and the edges are still joined. Default: `1`.
#'
#' @return A list with `mainstem_line` (an sfc with one LINESTRING, in the CRS
#'   of `dem`, running from headwater to outlet) and `length_m` (its length as
#'   a plain number).
#' @export
#' @seealso [extract_stream_profile()] for the elevation profile along this
#'   path, and [delineate_streamlines()] for the network.
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
#' extract_mainstem_path(flowlines, dem)

extract_mainstem_path <- function(flowlines, dem, snap_tol = 1) {

  if (!terra::same.crs(flowlines, dem)) {
    flowlines <- sf::st_transform(flowlines, terra::crs(dem))
  }

  # Normalize to simple LINESTRING features before building the network.
  flowlines_clean <- flowlines |>
    sf::st_geometry() |>
    sf::st_cast("MULTILINESTRING") |>
    sf::st_cast("LINESTRING") |>
    sf::st_as_sf()

  # 1. Build an undirected spatial network graph from the flowlines.
  net <- sfnetworks::as_sfnetwork(flowlines_clean, directed = FALSE)

  # 2. Restrict to the single largest connected component -- drops isolated/
  #    disconnected tributary fragments that can't reach the rest of the
  #    network (and that would otherwise break the headwater/outlet search
  #    below if the DEM's global max/min happened to fall on one of them).
  net <- net |>
    tidygraph::activate("nodes") |>
    dplyr::mutate(.component = tidygraph::group_components()) |>
    dplyr::filter(.component == names(which.max(table(.component)))) |>
    dplyr::select(-".component")

  # 3. Edge weights = geographic edge length, for Dijkstra's algorithm below.
  net <- net |>
    tidygraph::activate("edges") |>
    dplyr::mutate(.weight = sfnetworks::edge_length())

  # 4. Identify headwater (max elevation) and outlet (min elevation) nodes
  #    within this connected component, from the DEM.
  node_pts <- net |> tidygraph::activate("nodes") |> sf::st_as_sf()
  node_elev <- terra::extract(dem, terra::vect(node_pts))[[2]]

  if (all(is.na(node_elev))) {
    stop("extract_mainstem_path(): could not extract any DEM elevation at ",
         "network nodes -- check that `dem` covers the extent of `flowlines`.")
  }

  headwater_idx <- which.max(ifelse(is.na(node_elev), -Inf, node_elev))
  outlet_idx    <- which.min(ifelse(is.na(node_elev),  Inf, node_elev))

  if (headwater_idx == outlet_idx) {
    stop("extract_mainstem_path(): headwater and outlet nodes resolved to the ",
         "same network node -- check DEM coverage/resolution relative to the ",
         "flowline network.")
  }

  # 5. Dijkstra's shortest (edge-length-weighted) path from headwater to outlet.
  path <- sfnetworks::st_network_paths(
    net,
    from = headwater_idx,
    to = outlet_idx,
    weights = ".weight",
    type = "shortest"
  )

  edge_idx <- path$edge_paths[[1]]
  node_idx <- path$node_paths[[1]]

  if (length(edge_idx) == 0) {
    stop("extract_mainstem_path(): no connected path found between the ",
         "headwater and outlet nodes -- the network may be disconnected ",
         "between the DEM's highest and lowest points within this component.")
  }

  path_edges <- net |>
    tidygraph::activate("edges") |>
    sf::st_as_sf() |>
    dplyr::slice(edge_idx)

  start_coord <- sf::st_coordinates(node_pts[node_idx[1], ])[1, c("X", "Y")]

  mainstem_line <- stitch_path_edges(path_edges, start_coord, tol = snap_tol)
  mainstem_line <- sf::st_sfc(mainstem_line, crs = sf::st_crs(flowlines_clean))

  list(
    mainstem_line = mainstem_line,
    length_m = as.numeric(sf::st_length(mainstem_line))
  )
}


# Internal helper for extract_mainstem_path(). Takes the edges of a graph
# shortest path, already in path order (as returned by
# sfnetworks::st_network_paths()), and walks them into a single ordered
# LINESTRING, reversing any edge digitized against the direction of travel and
# warning if consecutive edges don't share an endpoint within `tol` (a sign of
# a small digitizing gap in the source network).
#   edges_ordered: sf object of LINESTRING edges, one row per path edge.
#   start_coord: numeric (X, Y) of the path's headwater node, used to orient
#     the first edge.
#   tol: endpoint-matching tolerance in CRS units.
# Returns a single sfg LINESTRING.
stitch_path_edges <- function(edges_ordered, start_coord, tol = 1) {

  n <- nrow(edges_ordered)
  coords_list <- vector("list", n)
  current_pt <- as.numeric(start_coord)

  for (i in seq_len(n)) {
    mat <- sf::st_coordinates(sf::st_geometry(edges_ordered)[[i]])[, c("X", "Y"), drop = FALSE]

    d_start <- sqrt(sum((mat[1, ] - current_pt)^2))
    d_end   <- sqrt(sum((mat[nrow(mat), ] - current_pt)^2))

    if (min(d_start, d_end) > tol) {
      warning("stitch_path_edges(): edge ", i, " does not share an endpoint ",
              "with the running path within tol = ", tol, " (closest gap = ",
              round(min(d_start, d_end), 2), " CRS units). Likely a small ",
              "digitizing gap in the source network -- consider snapping ",
              "vertices (e.g. sf::st_snap()) before extraction.")
    }

    if (d_end < d_start) {
      mat <- mat[rev(seq_len(nrow(mat))), , drop = FALSE]
    }

    if (i > 1) {
      mat <- mat[-1, , drop = FALSE]  # drop the vertex shared with the previous edge
    }

    coords_list[[i]] <- mat
    current_pt <- mat[nrow(mat), ]
  }

  all_coords <- do.call(rbind, coords_list)
  sf::st_linestring(all_coords)
}
