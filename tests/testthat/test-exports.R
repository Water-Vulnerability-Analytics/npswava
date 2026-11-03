test_that("the package exports its retrieval and analysis functions", {
  exports <- getNamespaceExports("npswava")
  expected <- c("get_pod_california", "get_pod_colorado", "get_pod_nevada",
                "get_pod_new_mexico", "get_pod_utah",
                "get_water_rights_montana", "get_water_suppliers_utah",
                "get_water_use_utah", "get_noaa", "get_snotel", "list_nwis",
                "get_climate_future", "get_climate_historic",
                "get_aoi_source", "make_ym", "calc_water_year",
                "baseflow_eckhardt", "select_cfs_deep_dive")
  expect_true(all(expected %in% exports))
})

test_that("functions that read the shared data folder fail clearly without it", {
  withr::local_options(wava.data_dir = file.path(tempdir(), "no-such-folder"))
  expect_error(get_pod_nevada())
})
