test_that("extract_mainstem_path() follows the highest branch through the confluence", {
  out <- extract_mainstem_path(synthetic_flowlines(), synthetic_dem())
  expect_named(out, c("mainstem_line", "length_m"))
  expect_s3_class(out$mainstem_line, "sfc_LINESTRING")
  expect_equal(length(out$mainstem_line), 1)
  # Headwater first, the confluence, then the outlet
  expect_equal(unname(sf::st_coordinates(out$mainstem_line)[, c("X", "Y")]),
               rbind(c(800, 950), c(500, 500), c(500, 100)))
  expect_equal(out$length_m, sqrt(300^2 + 450^2) + 400)
  expect_equal(sf::st_crs(out$mainstem_line), sf::st_crs(5070))
})

test_that("extract_mainstem_path() orients edges digitized against the flow", {
  flowlines <- synthetic_flowlines()
  reversed <- flowlines
  sf::st_geometry(reversed) <- sf::st_sfc(
    sf::st_linestring(rbind(c(500, 500), c(500, 100))),
    sf::st_linestring(rbind(c(200, 900), c(500, 500))),
    sf::st_linestring(rbind(c(800, 950), c(500, 500))),
    crs = 5070
  )
  expect_equal(
    unname(sf::st_coordinates(extract_mainstem_path(reversed, synthetic_dem())$mainstem_line)[, c("X", "Y")]),
    rbind(c(800, 950), c(500, 500), c(500, 100))
  )
})

test_that("extract_mainstem_path() keeps only the largest connected part", {
  flowlines <- synthetic_flowlines()
  # A fragment off to the side that reaches higher than the main network
  fragment <- sf::st_sf(id = 4L, geometry = sf::st_sfc(
    sf::st_linestring(rbind(c(50, 990), c(50, 950))), crs = 5070
  ))
  out <- extract_mainstem_path(rbind(flowlines, fragment), synthetic_dem())
  expect_equal(unname(sf::st_coordinates(out$mainstem_line)[1, c("X", "Y")]),
               c(800, 950))
})

test_that("extract_mainstem_path() reprojects flowlines to the DEM's CRS", {
  flowlines <- sf::st_transform(synthetic_flowlines(), 4326)
  out <- extract_mainstem_path(flowlines, synthetic_dem())
  expect_true(sf::st_crs(out$mainstem_line) == sf::st_crs(5070))
  expect_equal(out$length_m, sqrt(300^2 + 450^2) + 400, tolerance = 1e-6)
})

test_that("extract_mainstem_path() stops when the DEM misses the network", {
  flowlines <- synthetic_flowlines()
  sf::st_geometry(flowlines) <- sf::st_geometry(flowlines) + c(1e6, 1e6)
  sf::st_crs(flowlines) <- 5070
  expect_error(extract_mainstem_path(flowlines, synthetic_dem()),
               "could not extract any DEM elevation")
})

test_that("stitch_path_edges() joins edges and warns about a gap", {
  edges <- sf::st_sf(geometry = sf::st_sfc(
    sf::st_linestring(rbind(c(0, 0), c(10, 0))),
    sf::st_linestring(rbind(c(20, 0), c(10, 0)))
  ))
  expect_equal(unname(sf::st_coordinates(stitch_path_edges(edges, c(0, 0)))[, c("X", "Y")]),
               rbind(c(0, 0), c(10, 0), c(20, 0)))

  gap <- sf::st_sf(geometry = sf::st_sfc(
    sf::st_linestring(rbind(c(0, 0), c(10, 0))),
    sf::st_linestring(rbind(c(15, 0), c(20, 0)))
  ))
  expect_warning(stitch_path_edges(gap, c(0, 0)), "does not share an endpoint")
  expect_no_warning(stitch_path_edges(gap, c(0, 0), tol = 6))
})

test_that("extract_stream_profile() measures distance, elevation and slope from the headwater", {
  dem <- synthetic_dem()
  out <- extract_stream_profile(synthetic_flowlines(), dem, sample_dist = 50)
  expect_named(out, c("nodes", "mainstem_flowline", "profile_plot"))
  expect_s3_class(out$profile_plot, "ggplot")
  expect_s3_class(out$nodes, "sf")
  nodes <- out$nodes
  expect_equal(nodes$node_id, seq_len(nrow(nodes)))
  expect_equal(nodes$cum_dist_m[1], 0)
  expect_false(is.unsorted(nodes$cum_dist_m))
  expect_gt(nodes$elevation_m[1], nodes$elevation_m[nrow(nodes)])
  expect_equal(nodes$elevation_m, terra::extract(dem, terra::vect(nodes))[[2]])
  n <- nrow(nodes)
  expect_equal(nodes$dz[-n], nodes$elevation_m[-n] - nodes$elevation_m[-1])
  expect_equal(nodes$dx[-n], diff(nodes$cum_dist_m))
  expect_equal(nodes$slope_m_per_m, nodes$dz / nodes$dx)
  expect_equal(nodes$slope_pct, nodes$slope_m_per_m * 100)
  expect_true(is.na(nodes$slope_pct[n]))
})

test_that("extract_stream_profile() samples at the spacing it is given", {
  coarse <- extract_stream_profile(synthetic_flowlines(), synthetic_dem(),
                                   sample_dist = 100)
  fine <- extract_stream_profile(synthetic_flowlines(), synthetic_dem(),
                                 sample_dist = 20)
  expect_gt(nrow(fine$nodes), nrow(coarse$nodes))
  expect_equal(nrow(coarse$nodes), round((sqrt(300^2 + 450^2) + 400) / 100))
})

test_that("extract_stream_profile() stops on a channel shorter than min_reach_length", {
  expect_error(
    extract_stream_profile(synthetic_flowlines(), synthetic_dem(),
                           min_reach_length = 5000),
    "shorter than min_reach_length"
  )
})

test_that("delineate_streamlines() stops without a DEM to work from", {
  expect_error(delineate_streamlines(output_dir = tempfile("streams")),
               "No 'dem' provided")
})

test_that("delineate_streamlines() writes the files output_option asks for", {
  skip_without_whitebox("delineate_streamlines() output_option test")
  dem <- synthetic_dem()

  all_dir <- tempfile("streams")
  streams <- delineate_streamlines(dem, output_dir = all_dir, file_prefix = "t",
                                   threshold = 200)
  expect_s3_class(streams, "sf")
  expect_equal(sf::st_crs(streams), sf::st_crs("EPSG:5070"))
  expect_gt(nrow(streams), 0)
  expect_true(all(file.exists(file.path(
    all_dir, paste0("t_", c("dem.tif", "breached.tif", "flow_dir.tif",
                            "flow_accum.tif", "streams.gpkg"))
  ))))
  expect_false(file.exists(file.path(all_dir, "t_streams.shp")))

  flow_dir <- tempfile("streams")
  delineate_streamlines(dem, output_dir = flow_dir, file_prefix = "t",
                        threshold = 200, output_option = "flowlines_only")
  expect_equal(list.files(flow_dir), "t_streams.gpkg")

  shp_dir <- tempfile("streams")
  delineate_streamlines(dem, output_dir = shp_dir, file_prefix = "t",
                        threshold = 200, write_gpkg = FALSE)
  expect_true(file.exists(file.path(shp_dir, "t_streams.shp")))
  expect_false(file.exists(file.path(shp_dir, "t_streams.gpkg")))

  none_dir <- tempfile("streams")
  delineate_streamlines(dem, output_dir = none_dir, file_prefix = "t",
                        threshold = 200, output_option = "none")
  expect_false(file.exists(file.path(none_dir, "t_streams.gpkg")))
})

test_that("delineate_streamlines() reprojects, and clips to the aoi", {
  skip_without_whitebox("delineate_streamlines() aoi test")
  aoi <- sf::st_as_sf(sf::st_as_sfc(sf::st_bbox(
    c(xmin = 300, ymin = 200, xmax = 700, ymax = 800), crs = sf::st_crs(5070)
  )))
  # st_intersection() warns that attributes are assumed constant
  streams <- suppressWarnings(
    delineate_streamlines(synthetic_dem(), aoi = aoi,
                          output_dir = tempfile("streams"),
                          threshold = 50, aoi_buffer_dist = 50)
  )
  expect_gt(nrow(streams), 0)
  bbox <- sf::st_bbox(streams)
  expect_gte(bbox[["xmin"]], 300)
  expect_lte(bbox[["xmax"]], 700)
  expect_gte(bbox[["ymin"]], 200)
  expect_lte(bbox[["ymax"]], 800)

  utm <- delineate_streamlines(synthetic_dem(), output_dir = tempfile("streams"),
                               threshold = 200, crs_out = "EPSG:32612",
                               output_option = "none")
  expect_equal(sf::st_crs(utm), sf::st_crs("EPSG:32612"))
})

test_that("delineate_streamlines() can resume from saved files, and return the main channel", {
  skip_without_whitebox("delineate_streamlines() resume test")
  out_dir <- tempfile("streams")
  first <- delineate_streamlines(synthetic_dem(), output_dir = out_dir,
                                 file_prefix = "t", threshold = 200,
                                 mainline_only = FALSE)

  # No dem: everything comes from the files saved by the first run
  expect_message(
    resumed <- delineate_streamlines(output_dir = out_dir, file_prefix = "t",
                                     threshold = 200),
    "Resuming from saved DEM"
  )
  expect_equal(sf::st_geometry(resumed), sf::st_geometry(first))

  main <- delineate_streamlines(output_dir = out_dir, file_prefix = "t",
                                threshold = 200, mainline_only = TRUE)
  expect_equal(nrow(main), 1)
  expect_true(all(c("segment_id", "length_m") %in% names(main)))
  expect_equal(main$segment_id, 1)
  expect_equal(main$length_m, as.numeric(sf::st_length(main)))
})
