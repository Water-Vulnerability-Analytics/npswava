#' @title Download daily MACA climate projections for points or polygons
#'
#' @description
#' Downloads daily MACA downscaled climate projections with
#' [climateR::getMACA()] for each feature of an sf object and each model and
#' scenario, so it needs internet, and returns a long table of precipitation
#' and temperature per grid cell and day. Date ranges that span 2005 are
#' downloaded in two parts (historical forcing up to 2005, the scenario
#' from 2006).
#'
#' * Polygons: every MACA cell in the polygon, one row per cell and day (see
#'   `exact`).
#' * Points: the cell containing the point, one row per day, with the point
#'   as `x` and `y`.
#'
#' Temperatures are converted from kelvin to degrees C (`tmax_C`, `tmin_C`
#' and `tmean_C`, their mean) and precipitation is returned as `ppt_mm`.
#' This needs `vars` to include `"tasmax"`, `"tasmin"` and `"pr"`. Other MACA
#' variables are `rhsmax`, `rhsmin`, `huss`, `rsds`, `was`, `uas` and `vas`.
#'
#' @param sf sf object. The areas of interest, all polygons/multipolygons or
#'   all points. Stops with an error for other geometry types. Required.
#' @param col_name String. The name of the column of `sf` that identifies
#'   each feature; it is kept in the output under the same name. Give it as
#'   a string: a bare name fails when the column is renamed back. Required.
#' @param start String, `"YYYY-MM-DD"`. The first day to download. Default:
#'   `"2023-01-01"`.
#' @param end String, `"YYYY-MM-DD"`. The last day to download. Default:
#'   `"2070-01-01"`.
#' @param vars Character vector. The MACA variables to download. Default:
#'   `c("tasmax", "tasmin", "pr")`.
#' @param GCM Character vector. The model and scenario pairs, as
#'   `"<model>.<scenario>"`, e.g. `"HadGEM2-CC365.rcp45"`.
#'   * Model and scenario pairs: one download per pair.
#'   * Any element `"all"`: the 40 pairs (20 models, RCP4.5 and RCP8.5) used
#'     by the NPS.
#'
#'   Default: `select_cfs$GCM`, the models picked by [select_cfs_deep_dive()]
#'   in an object named `select_cfs` in the global environment; pass `GCM`
#'   when there is none.
#' @param exact Logical. How polygons are sampled; ignored for points.
#'   * `TRUE`: downloads a 4 km buffer around each polygon and reads the
#'     cells at the `gridmet_points`, then gives each value the coordinates
#'     of the nearest GridMET point. The future and historical tables then
#'     share the same `x` and `y`, so they join cleanly.
#'   * `FALSE`: keeps only the cells whose center is inside the polygon, with
#'     the MACA cell centers as `x` and `y`.
#'
#'   Default: `FALSE`.
#' @param gridmet_points sf object or `NULL`. The GridMET cell points used
#'   for the historical data, with numeric `x` and `y` columns (longitude and
#'   latitude), e.g. from the output of [get_climate_historic()].
#'   * An sf object: required when `exact = TRUE`.
#'   * `NULL`: only allowed with `exact = FALSE`.
#'
#'   Default: `NULL`.
#'
#' @return A tibble, one row per feature, model, scenario, cell and day,
#'   with `x`, `y`, `date`, `GCM`, `Ensemble`, `RCP`, `ppt_mm`, `tmax_C`,
#'   `tmin_C`, `tmean_C` and the `col_name` column.
#' @export
#' @seealso [get_climate_historic()] for GridMET at the same places;
#'   [select_cfs_deep_dive()] to pick the models.
#' @examplesIf interactive()
#' # Downloads from the MACA server, so it needs internet
#' site <- sf::st_as_sf(data.frame(site = "Springdale", lon = -112.9877, lat = 37.1889),
#'                      coords = c("lon", "lat"), crs = 4326)
#' future <- get_climate_future(site, col_name = "site",
#'                              start = "2040-01-01", end = "2040-01-31",
#'                              GCM = c("CCSM4.rcp45", "CCSM4.rcp85"))
#' head(future)

get_climate_future <- function(sf,
                               col_name,
                               start = "2023-01-01",
                               end = "2070-01-01",
                               vars = c("tasmax", "tasmin", "pr"),
                               GCM = select_cfs$GCM,
                               exact = FALSE,
                               gridmet_points = NULL) {
  
  if(any(GCM == "all")){
    
    GCM  <- c("HadGEM2-ES365.rcp45", 
              "HadGEM2-ES365.rcp85",
              "HadGEM2-CC365.rcp45", 
              "HadGEM2-CC365.rcp85", 
              "CNRM-CM5.rcp45",      
              "CNRM-CM5.rcp85",
              "CCSM4.rcp45", 
              "CCSM4.rcp85",
              "CanESM2.rcp45",
              "CanESM2.rcp85",       
              "MIROC5.rcp45",
              "MIROC5.rcp85",
              "IPSL-CM5A-MR.rcp45",
              "IPSL-CM5A-MR.rcp85", 
              "NorESM1-M.rcp45",     
              "NorESM1-M.rcp85",
              "bcc-csm1-1-m.rcp45",
              "bcc-csm1-1-m.rcp85",
              "IPSL-CM5A-LR.rcp45",
              "IPSL-CM5A-LR.rcp85",  
              "CSIRO-Mk3-6-0.rcp45",
              "CSIRO-Mk3-6-0.rcp85", 
              "BNU-ESM.rcp45",
              "BNU-ESM.rcp85",
              "MRI-CGCM3.rcp45",     
              "MRI-CGCM3.rcp85",
              "GFDL-ESM2M.rcp45", 
              "GFDL-ESM2M.rcp85",
              "GFDL-ESM2G.rcp45",
              "GFDL-ESM2G.rcp85",    
              "MIROC-ESM.rcp45", 
              "MIROC-ESM.rcp85", 
              "inmcm4.rcp45", 
              "inmcm4.rcp85",
              "MIROC-ESM-CHEM.rcp45",
              "MIROC-ESM-CHEM.rcp85",
              "bcc-csm1-1.rcp45",
              "bcc-csm1-1.rcp85", 
              "IPSL-CM5B-LR.rcp45", 
              "IPSL-CM5B-LR.rcp85") 
    
  }
  
  sf <- sf %>%
    dplyr::rename("join_index" = {{col_name}})
  
  all_climate_data <- vector("list", length = nrow(sf) * length(GCM))
  idx <- 0
  models <- stringr::str_extract(GCM, "^[^.]+")     # Get models to run
  scenarios <- stringr::str_extract(GCM, "(?<=\\.).*")     # Get scenarios (rcps) 
  
  if(any(unique(sf::st_geometry_type(sf)) %in% c("POLYGON", "MULTIPOLYGON"))) {
    
    for (i in 1:nrow(sf)) {
      
      aoi <- sf[i,]
      
      for (j in 1:length(GCM)) {
        
        idx <- idx + 1
        
        print(paste0('Downloading MACA ', models[j], '-', scenarios[j], ' for ', aoi$join_index, "."))
        
        if(exact == TRUE) {
          # download climate data using a 4km buffer to ensure full cell capture at aoi edges
          # getGridMET does NOT always grab all the grid cells that touch an AOI
          pull_aoi <- sf::st_buffer(aoi, 4000)
        } else {
          # download climate data NOT using a 4km buffer to ensure full cell capture at aoi edges
          # (how it was done in BRCA, VALL)
          pull_aoi <- aoi
        }
        
        if (lubridate::year(as.Date(start)) <= 2005 & lubridate::year(as.Date(end)) > 2005) {
          
          # 2005 marks the start of RCP forcings. If start and end date span
          # 2005-2006, need to pull in separately
          clim_pre <- climateR::getMACA(AOI = pull_aoi, 
                                        varname = vars,
                                        model = models[j],
                                        scenario = scenarios[j],
                                        startDate = start,
                                        endDate = "2005-12-31") #%>%
          #  rename_with(
          #    .fn = ~ stringr::str_replace(., "historical", scenarios[j]),
          #    .cols = contains("historical") 
          #  )
          
          clim_post <- climateR::getMACA(AOI = pull_aoi, 
                                         varname = vars,#,"pet","vs"),
                                         model = models[j],
                                         scenario = scenarios[j],
                                         startDate = "2006-01-01",
                                         endDate = end) 
          
          clim <- c(clim_pre, clim_post) 
        
        } else {
          
          clim <- climateR::getMACA(AOI = pull_aoi, 
                                    varname = vars,
                                    model = models[j],
                                    scenario = scenarios[j],
                                    startDate = start,
                                    endDate = end) 
          
        }
        
        # If the result is a spatraster (i.e., AOI spanned more than 1 grid cell)
        if(!is.data.frame(clim) | inherits(clim[[1]], "SpatRaster")) {
          
          clim_crs <- sf::st_crs(clim[[1]])
          
          if(exact == TRUE){
            # transform points to match raster CRS if needed
            if(sf::st_crs(gridmet_points) != clim_crs) {
              gridmet_points_trans <- sf::st_transform(gridmet_points, crs = clim_crs)
            } else {
              gridmet_points_trans <- gridmet_points
            }
            # crop with extract to grab all touching 
            clim <- clim %>%
              purrr::map(~terra::extract(., terra::vect(gridmet_points_trans), touches = TRUE, xy = TRUE, ID = FALSE))
            
          } else if(exact == FALSE){
            # transform AOI to match raster CRS if needed
            if(sf::st_crs(aoi) != clim_crs) {
              aoi_trans <- sf::st_transform(aoi, crs = clim_crs)
            } else {
              aoi_trans <- aoi
            }
            # otherwise original method using crop (only complete grids returned)
            clim <- clim %>%
              purrr::map(~terra::crop(., aoi_trans, mask = TRUE))
            
          }
          
          all_climate_data[[idx]] <- clim %>%
            purrr::map_dfr(~ terra::as.data.frame(., xy = TRUE)) %>%
            data.table::data.table() %>%
            tidyr::pivot_longer(-(x:y),
                                names_to = "var_temp",
                                values_to = "val") %>%
            tidyr::separate_wider_delim(var_temp, "_", 
                                        names = c("var","date","GCM",
                                                  "Ensemble","RCP")) %>%
            tidyr::drop_na(val) %>%
            dplyr::group_by(x, y, date) %>%
            tidyr::pivot_wider(names_from = "var", 
                               values_from = "val") %>%
            dplyr::mutate(date = as.Date(date),
                          ppt_mm = pr,
                          tmax_C = tasmax - 273.15,
                          tmin_C = tasmin - 273.15,
                          tmean_C = (tmax_C + tmin_C)/2,
                          join_index = aoi$join_index) %>%
            dplyr::select(-c("tasmax","tasmin","pr")) %>%
            dplyr::mutate(RCP = stringr::str_replace(RCP, "historical", scenarios[j]))
          
        } else {
          
          all_climate_data[[idx]] <- clim %>%
            data.table::data.table() %>%
            # names of columns include va_mode_rcp so must rename
            dplyr::rename_with(~ stringr::str_split(.x, "_", n = 2) %>% purrr::map_chr(1)) %>%
            # since polygon grabbed a single grid, does not provide the coordinates
            # of the gridMET cell, so we fill in x and y with the coordinates
            # of the sf object:
            dplyr::mutate(x = sf::st_coordinates(aoi)[[1]],
                          y = sf::st_coordinates(aoi)[[2]]) %>%
            # Then do all other cleaning steps done for polygon sf objects:
            dplyr::mutate(date = as.Date(date),
                          ppt_mm = pr,
                          tmax_C = tasmax - 273.15,
                          tmin_C = tasmin - 273.15,
                          tmean_C = (tmax_C + tmin_C)/2,
                          join_index = aoi$join_index,
                          GCM = models[j],
                          RCP = scenarios[j],
                          Ensemble = "r1i1p1") %>%
            dplyr::select(-c("tasmax","tasmin","pr"))
          
        }
      }
    }
    
    all_climate_data <- all_climate_data %>%
      dplyr::bind_rows()
    
    
    if(exact == TRUE){
      
      points <- all_climate_data %>%
        dplyr::group_by(x, y) %>%
        dplyr::summarise(.groups = "drop") %>%
        sf::st_as_sf(coords = c("x", "y"), crs = 4326, remove = FALSE) %>%
        dplyr::select("old_x" = x, "old_y" = y) 
      
      # ensure gridmet_points has coords & is POINT sf
      if (sf::st_crs(gridmet_points)$epsg != 4326){
        
        gridmet_points <- sf::st_transform(gridmet_points, 4326)
      }
      
      # for each row in `points`, find the index of its single nearest historical point
      nn_idx <- sf::st_nearest_feature(points, gridmet_points)  # length == nrow(points)
      
      # buffer by 800 m safely (transform to a meters-based CRS)
      points <- points %>%
        dplyr::mutate(rowid = nn_idx) %>%
        dplyr::left_join(sf::st_drop_geometry(gridmet_points) %>% tibble::rowid_to_column() %>% dplyr::select(x, y, rowid),
                  by = "rowid") %>%
        sf::st_drop_geometry() %>%
        dplyr::select(-rowid)
      
      all_climate_data <- all_climate_data %>%
        dplyr::left_join(points, by = c("x" = "old_x", "y" = "old_y")) %>%
        dplyr::ungroup() %>%
        dplyr::select(-c(x,y)) %>%
        dplyr::rename(x = x.y, y = y.y)
      
      
    }
    
    # Rename the join_index column
    colnames(all_climate_data)[colnames(all_climate_data) == "join_index"] <- {{col_name}}
    
    return(all_climate_data)
    
    
    # IF the input geometry is a point...
  } else if(unique(sf::st_geometry_type(sf)) == "POINT") {
    
    for (i in 1:nrow(sf)) {
      
      aoi <- sf[i,]
      
      for (j in 1:length(GCM)) {
        
        idx <- idx + 1
        
        print(paste0('Downloading MACA for ', aoi$join_index, "."))
        
        if (lubridate::year(as.Date(start)) <= 2005 & lubridate::year(as.Date(end)) > 2005) {
          
          # 2005 marks the start of RCP forcings. If start and end date span
          # 2005-2006, need to pull in separately
          clim_pre <- climateR::getMACA(AOI = aoi, 
                                        varname = vars,#,"pet","vs"),
                                        model = models[j],
                                        scenario = scenarios[j],
                                        startDate = start,
                                        endDate = "2005-12-31") %>%
            dplyr::rename_with(
              .fn = ~ stringr::str_replace(., "historical", scenarios[j]),
              .cols = dplyr::contains("historical") 
            )
          
          clim_post <- climateR::getMACA(AOI = aoi, 
                                         varname = vars,#,"pet","vs"),
                                         model = models[j],
                                         scenario = scenarios[j],
                                         startDate = "2006-01-01",
                                         endDate = end) 
          
          clim <- clim_pre %>%
            dplyr::bind_rows(clim_post) 
          
        } else {
        
        clim <- climateR::getMACA(AOI = aoi, 
                                  varname = vars,#,"pet","vs"),
                                  model = models[j],
                                  scenario = scenarios[j],
                                  startDate = start,
                                  endDate = end) 
        }
        
        all_climate_data[[idx]] <- clim %>%
          data.table::data.table() %>%
          # names of columns include va_mode_rcp so must rename
          dplyr::rename_with(~ stringr::str_split(.x, "_", n = 2) %>% purrr::map_chr(1)) %>%
          # since point pulls from gridMET do not provide the coordinates
          # of the gridMET cell, we fill in x and y with the coordinates
          # of the sf object:
          dplyr::mutate(x = sf::st_coordinates(aoi)[[1]],
                        y = sf::st_coordinates(aoi)[[2]]) %>%
          # Then do all other cleaning steps done for polygon sf objects:
          dplyr::mutate(date = as.Date(date),
                        ppt_mm = pr,
                        tmax_C = tasmax - 273.15,
                        tmin_C = tasmin - 273.15,
                        tmean_C = (tmax_C + tmin_C)/2,
                        join_index = aoi$join_index,
                        GCM = models[j],
                        RCP = scenarios[j],
                        Ensemble = "r1i1p1") %>%
          dplyr::select(-c("tasmax","tasmin","pr"))
        
      }
    }
    
    all_climate_data <- all_climate_data %>%
      dplyr::bind_rows()
    
    # Rename the join_index column
    colnames(all_climate_data)[colnames(all_climate_data) == "join_index"] <- {{col_name}}
    
    return(all_climate_data)
    
  } else {
    stop("Your sf feature is neither a polygon nor point feature, or it needs to be made valid.")
  }
  
}




