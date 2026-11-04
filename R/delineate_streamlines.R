#' @title Delineate a stream network from a DEM
#'
#' @description
#' Runs a WhiteboxTools workflow on a digital elevation model: breaches
#' depressions, computes D8 flow direction and flow accumulation, extracts
#' streams above a cell threshold and converts them to lines. The lines can be
#' clipped to an area of interest and reduced to the main channel. It needs
#' the WhiteboxTools binary, which [whitebox::install_whitebox()] installs.
#'
#' Files are written to `output_dir`, named `<file_prefix>_dem.tif`,
#' `_breached.tif`, `_flow_dir.tif`, `_flow_accum.tif`, `_streams.tif`,
#' `_streams.shp` and `_streams.gpkg`.
#'
#' @param dem SpatRaster or `NULL`. The input DEM.
#'   * A SpatRaster: reprojected to `crs_out` if needed, cropped and masked to
#'     the buffered `aoi` if there is one, and saved to `output_dir`.
#'   * `NULL`: `output_dir` must already hold `<file_prefix>_dem.tif` and
#'     `resume` must be `TRUE`, otherwise the function stops.
#'
#'   Default: `NULL`.
#' @param aoi sf, SpatVector or `NULL`. The area of interest.
#'   * A polygon: the DEM is restricted to it plus `aoi_buffer_dist`, and the
#'     output lines are clipped to the polygon itself.
#'   * `NULL`: the whole DEM is used.
#'
#'   Default: `NULL`.
#' @param output_dir String. The folder for the files, created if missing.
#'   With `resume = TRUE` it is also where saved files are looked for.
#'   Required.
#' @param file_prefix String. The start of every file name. Default:
#'   `"streams"`.
#' @param threshold Number. The flow accumulation, in cells, needed to start a
#'   stream channel. Default: `1000`.
#' @param breach_dist Number. The longest breach channel, in cells, passed to
#'   [whitebox::wbt_breach_depressions_least_cost()]. Default: `10`.
#' @param aoi_buffer_dist Number. The buffer around `aoi`, in the DEM's CRS
#'   units, used for flow processing to avoid edge effects. Default: `500`.
#' @param mainline_only Logical. Which lines are returned.
#'   * `TRUE`: only the main channel, from [extract_mainstem_path()], with
#'     `segment_id` and `length_m` columns.
#'   * `FALSE`: the whole network.
#'
#'   Default: `FALSE`.
#' @param resume Logical. Whether to reuse saved files in `output_dir`.
#'   * `TRUE`: a saved `<file_prefix>_dem.tif` is used in place of `dem`, and
#'     the breaching and flow steps are skipped where their files exist. A
#'     saved DEM wins over `dem` and over a new `aoi` for the DEM's extent.
#'   * `FALSE`: every step runs again.
#'
#'   Stream extraction always runs, so a new `threshold` takes effect either
#'   way. Default: `TRUE`.
#' @param output_option String. What is done with the files at the end.
#'   * `"all"`: the DEM, breached, flow direction and flow accumulation rasters
#'     stay. See `write_gpkg` for the stream files.
#'   * `"flowlines_only"`: those rasters and the stream raster are deleted. See
#'     `write_gpkg` for the stream files.
#'   * `"none"`: no GeoPackage is written, and nothing is deleted, so all the
#'     rasters and the shapefile stay in `output_dir`.
#'
#'   Default: `"all"`.
#' @param write_gpkg Logical. Whether the lines are saved as a GeoPackage,
#'   unless `output_option` is `"none"`.
#'   * `TRUE`: writes `<file_prefix>_streams.gpkg` and deletes every other
#'     `<file_prefix>_streams.*` file: the shapefile and the stream raster.
#'   * `FALSE`: no GeoPackage, and the shapefile stays.
#'
#'   Default: `TRUE`.
#' @param overwrite Logical. What happens when the GeoPackage already exists.
#'   The rasters and shapefile are always replaced.
#'   * `TRUE`: it is replaced.
#'   * `FALSE`: [sf::st_write()] stops with an error, after the rasters and
#'     shapefile have been written.
#'
#'   Default: `TRUE`.
#' @param crs_out String. The EPSG code or PROJ string for the DEM and the
#'   output lines. Default: `"EPSG:5070"`.
#'
#' @return An sf object of stream lines in `crs_out`.
#' @export
#' @seealso [extract_mainstem_path()] for the main channel, and
#'   [extract_stream_profile()] for its longitudinal profile.
#' @examplesIf whitebox::check_whitebox_binary()
#' # A valley running south, falling 1 m every 10 m
#' dem <- terra::rast(xmin = 0, xmax = 1000, ymin = 0, ymax = 1000,
#'                    resolution = 10, crs = "EPSG:5070")
#' terra::values(dem) <- 2000 + terra::yFromCell(dem, seq_len(terra::ncell(dem))) / 10 +
#'   abs(terra::xFromCell(dem, seq_len(terra::ncell(dem))) - 500) / 20
#'
#' streams <- delineate_streamlines(dem, output_dir = tempfile("streams"),
#'                                  threshold = 200)
#' streams

delineate_streamlines <- function(dem = NULL,
                                  aoi = NULL,
                                  output_dir,
                                  file_prefix = "streams",
                                  threshold = 1000,
                                  breach_dist = 10,
                                  aoi_buffer_dist = 500,
                                  mainline_only = FALSE,
                                  resume = TRUE,
                                  output_option = c("all", "flowlines_only", "none"),
                                  write_gpkg = TRUE,
                                  overwrite = TRUE,
                                  crs_out = "EPSG:5070") {
  
  output_option <- match.arg(output_option)
  
  if (is.null(output_dir)) {
    stop("An 'output_dir' must be specified to store or read intermediate inputs.")
  }
  
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  # File paths
  dem_path         <- file.path(output_dir, paste0(file_prefix, "_dem.tif"))
  breached_path    <- file.path(output_dir, paste0(file_prefix, "_breached.tif"))
  flow_dir_path    <- file.path(output_dir, paste0(file_prefix, "_flow_dir.tif"))
  flow_accum_path  <- file.path(output_dir, paste0(file_prefix, "_flow_accum.tif"))
  stream_raster_p  <- file.path(output_dir, paste0(file_prefix, "_streams.tif"))
  stream_vector_p  <- file.path(output_dir, paste0(file_prefix, "_streams.shp"))
  
  # --------------------------------------------------------------------------
  # 1. DEM Import & Preprocessing Logic
  # --------------------------------------------------------------------------
  if (resume && file.exists(dem_path)) {
    message("--> Resuming from saved DEM: ", dem_path)
    dem <- terra::rast(dem_path)
  } else {
    if (is.null(dem)) {
      stop("No 'dem' provided and no existing DEM raster found at: ", dem_path)
    }
    
    message("--> Processing original DEM (reprojecting/cropping)...")
    
    if (!terra::same.crs(dem, crs_out)) {
      dem <- terra::project(dem, crs_out)
    }
    
    if (!is.null(aoi)) {
      aoi_v <- terra::vect(aoi) |> 
        terra::project(terra::crs(dem))
      
      aoi_buf <- terra::buffer(aoi_v, width = aoi_buffer_dist)
      
      dem <- terra::crop(dem, aoi_buf) |> 
        terra::mask(aoi_buf)
    }
    
    terra::writeRaster(dem, dem_path, overwrite = TRUE)
  }
  
  # --------------------------------------------------------------------------
  # 2. Whitebox Hydro Processing Workflow
  # --------------------------------------------------------------------------
  if (!resume || !file.exists(breached_path)) {
    message("--> Breaching depressions...")
    whitebox::wbt_breach_depressions_least_cost(
      dem = dem_path,
      output = breached_path,
      dist = breach_dist
    )
  }
  
  if (!resume || !file.exists(flow_dir_path) || !file.exists(flow_accum_path)) {
    message("--> Computing D8 flow pointer and accumulation...")
    whitebox::wbt_d8_pointer(
      dem = breached_path,
      output = flow_dir_path
    )
    
    whitebox::wbt_d8_flow_accumulation(
      input = breached_path,
      output = flow_accum_path,
      out_type = "cells"
    )
  }
  
  # --------------------------------------------------------------------------
  # 3. Stream Channel Extraction
  # --------------------------------------------------------------------------
  message("--> Extracting streams with cell threshold = ", threshold)
  whitebox::wbt_extract_streams(
    flow_accum = flow_accum_path,
    output = stream_raster_p,
    threshold = threshold
  )
  
  whitebox::wbt_raster_streams_to_vector(
    streams = stream_raster_p,
    d8_pntr = flow_dir_path,
    output = stream_vector_p
  )
  
  # Read vector streams into R
  streams_sf <- sf::st_read(stream_vector_p, quiet = TRUE) |>
    sf::st_set_crs(terra::crs(dem)) |>
    sf::st_transform(crs_out)
  
  # Clip precisely to exact AOI polygon boundary
  if (!is.null(aoi)) {
    aoi_sf <- sf::st_as_sf(aoi) |> sf::st_transform(crs_out)
    streams_sf <- sf::st_intersection(streams_sf, sf::st_union(aoi_sf))
  }
  
  # --------------------------------------------------------------------------
  # 4. Mainline Filtering (If mainline_only = TRUE)
  # --------------------------------------------------------------------------
  if (mainline_only) {
    message("--> Isolating longest connected mainstem flowpath (graph shortest-path)...")
    
    # Graph shortest-path mainstem extraction (see R/extract_mainstem_path.R).
    # Replaces a naive st_combine()+st_line_merge()+pick-longest-piece approach,
    # which only merges through degree-2 nodes and therefore fractures -- and
    # picks the wrong "longest" piece -- at every confluence (tributary
    # junction). `dem` here is still the unbreached, (re)projected raster from
    # step 1 above -- whitebox's breach/flow-accum/streams outputs are written
    # to disk by file path and never reassigned into this R object.
    mainstem_result <- extract_mainstem_path(streams_sf, dem)
    mainstem_line <- sf::st_transform(mainstem_result$mainstem_line, crs_out)
    
    streams_sf <- sf::st_sf(
      geometry = mainstem_line,
      crs = crs_out
    ) |> 
      dplyr::mutate(segment_id = 1, length_m = mainstem_result$length_m)
  }
  
  # --------------------------------------------------------------------------
  # 5. Cleanup & File Saving Logic
  # --------------------------------------------------------------------------
  if (output_option == "flowlines_only") {
    rasters_to_remove <- c(dem_path, breached_path, flow_dir_path, flow_accum_path, stream_raster_p)
    file.remove(rasters_to_remove[file.exists(rasters_to_remove)])
  }
  
  if (output_option %in% c("all", "flowlines_only")) {
    if (write_gpkg) {
      gpkg_path <- file.path(output_dir, paste0(file_prefix, "_streams.gpkg"))
      sf::st_write(
        obj = streams_sf,
        dsn = gpkg_path,
        delete_dsn = overwrite,
        quiet = TRUE
      )
      shp_files <- list.files(output_dir, pattern = paste0("^", file_prefix, "_streams\\."), full.names = TRUE)
      shp_files <- shp_files[!grepl("\\.gpkg$", shp_files)]
      file.remove(shp_files)
    }
  }
  
  return(streams_sf)
}
