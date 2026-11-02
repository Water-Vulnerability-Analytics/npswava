#' @title Inventory of USGS NWIS data for an area, sites or states
#'
#' @description
#' Lists the data available from the USGS National Water Information System
#' (<https://waterdata.usgs.gov/nwis>) with [dataRetrieval::whatNWISdata()],
#' so it needs internet: one row per site, data type and parameter, with the
#' period of record and the number of observations. Parameter names come
#' from [dataRetrieval::parameterCdFile] and site type names from
#' [dataRetrieval::read_waterdata_metadata()].
#'
#' Give one of `aoi`, `sites` or `states`. If more than one is given, `aoi`
#' is used first, then `sites`, then `states`. If none is given, the function
#' stops with the error "BROKEN".
#'
#' @param aoi sf object or `NULL`. The area of interest, as polygons, e.g. a
#'   park boundary from [wavacore::getParkBoundary()].
#'   * An sf object: queries the bounding box of each feature (after the
#'     `dist` buffer) and keeps the sites inside the buffered area, with the
#'     `aoi` columns joined on.
#'   * `NULL`: uses `sites` or `states` instead.
#'
#'   Default: `NULL`.
#' @param dist Numeric. The distance to buffer `aoi` by, in meters for a
#'   longitude/latitude CRS (like the park boundaries) or in the CRS units
#'   otherwise; `0` for no buffer. Has no default, so it is required when
#'   `aoi` is given; ignored otherwise.
#' @param states Character vector or `NULL`. Two-letter state codes, e.g.
#'   `"UT"`; every site in those states is listed, in requests of up to
#'   35,000 sites. Only used when `aoi` and `sites` are `NULL`. Default:
#'   `NULL`.
#' @param sites Character vector or `NULL`. USGS site numbers, e.g.
#'   `"09405500"`, listed in requests of up to 35,000. Only used when `aoi` is
#'   `NULL`. Default: `NULL`.
#'
#' @return An sf object of site points, one row per site, data type and
#'   parameter, with `site_no`, `site_name`, `data_type_cd`, `site_type_cd`,
#'   `n_obs`, `begin_date`, `end_date`, `parameter` (name), `code` (parameter
#'   code), `site_type` and `data_type` (e.g. `"Daily"`, `"Groundwater
#'   Levels"`). In the CRS of `aoi` when it is given, otherwise NAD83. With
#'   `sites` or `states` the result is returned invisibly, so assign it.
#' @export
#' @seealso [add_gw_meta()] to add well and aquifer details to groundwater
#'   sites; [nldi_finder()] for the drainage basin of a gage;
#'   [baseflow_eckhardt()] to separate baseflow from streamflow.
#' @examplesIf interactive()
#' # Queries NWIS, so it needs internet
#' gage <- listNWIS(sites = "09405500")
#' gage[, c("site_no", "site_name", "data_type", "parameter", "begin_date")]
#'
#' # Every site within 2 km of a park
#' zion <- wavacore::getParkBoundary("ZION")
#' zion_sites <- listNWIS(aoi = zion, dist = 2000)
#' table(zion_sites$data_type)

listNWIS <- function(aoi = NULL, dist, states = NULL, sites = NULL){
  
  #tables <- rvest::read_html('https://help.waterdata.usgs.gov/parameter_cd?group_cd=%') %>%
  #  rvest::html_nodes('table') %>%
  #  rvest::html_table()
  
  #pcodes <- tables[[1]] %>%
  #  janitor::clean_names() %>%
  #  dplyr::mutate(parm_cd = stringr::str_pad(as.character(parameter_code), 5, pad = "0"))
  
  pcodes <- dataRetrieval::parameterCdFile %>% 
    janitor::clean_names() %>%
    dplyr::mutate(parm_cd = stringr::str_pad(as.character(parameter_cd), 5, pad = "0"))
  
  # site_url <- 'https://maps.waterdata.usgs.gov/mapper/help/sitetype.html'
  # 
  # table <- rvest::read_html(site_url) %>%
  #   rvest::html_nodes('table') %>%
  #   rvest::html_table()
  # 
  # table <- rbind(table[[1]], table[[2]], table[[3]], table[[4]], table[[5]]) %>%
  #   dplyr::select(site_type_cd = 1,
  #                 site_type = 2)
  
  table <- dataRetrieval::read_waterdata_metadata("site-types") %>%
    dplyr::select(site_type_cd = 1,
                  site_type = 3)
  
  # Grab NWIS by an area of interest:
  if(!is.null(aoi)){
    
    # add buffer around area of interest
    aoi_buffer <- aoi %>% 
      sf::st_buffer(., dist = dist)
    
    gage_sites <- vector("list", length = nrow(aoi_buffer))
    
    for (i in 1:nrow(aoi_buffer)){
      
      bbox <- sf::st_bbox(aoi_buffer[i,]) %>%
        as.vector() %>%
        round(., digits = 7) %>% 
        paste(collapse = ",")
      
      gage_sites[[i]] <- dataRetrieval::whatNWISdata(bBox = bbox)
      
    }
    
    gage_sites <- dplyr::bind_rows(gage_sites) %>%
      sf::st_as_sf(coords = c('dec_long_va', 'dec_lat_va'), crs = 4269)
    
    if(sf::st_crs(aoi) != sf::st_crs(gage_sites)){
      
      gage_sites <- sf::st_transform(gage_sites, sf::st_crs(aoi))
      
    }
    
    inventory <- gage_sites %>%
      dplyr::left_join(pcodes, by = "parm_cd") %>%
      .[aoi_buffer,] %>%
      dplyr::select(c(site_no,
                      site_name = station_nm,
                      data_type_cd,
                      site_type_cd = site_tp_cd,
                      n_obs = count_nu,
                      begin_date,
                      end_date,
                      #parameter = parameter_name_description,
                      parameter = parameter_nm,
                      code = parm_cd)) %>%
      dplyr::left_join(.,table,by = 'site_type_cd') %>%
      dplyr::mutate(data_type = dplyr::case_when(data_type_cd == "dv" ~ "Daily",
                                                 data_type_cd == "uv" ~ "Unit",
                                                 data_type_cd == "qw" ~ "Water Quality",
                                                 data_type_cd == "gw" ~ "Groundwater Levels",
                                                 data_type_cd == "iv" ~ "Unit",
                                                 data_type_cd == "sv" ~ "Site Visits",
                                                 data_type_cd == "pk" ~ "Peak Measurements",
                                                 data_type_cd == "ad" ~ "USGS Annual Water Data Report",
                                                 data_type_cd == "aw" ~ "Active Groundwater Level Network",
                                                 data_type_cd == "id" ~ "Historic Instantaneous")) %>%
      sf::st_join(., dplyr::select(aoi_buffer))
    
    return(inventory)
    
    # Grab specific NWIS sites:
  } else if(!is.null(sites)){
    
    # Check and split 'sites' into chunks if necessary
    # dataRetrieval crashes if too big:
    site_chunks <- if (length(sites) > 35000) {
      split(sites, ceiling(seq_along(sites) / 35000))
    } else {
      list(sites)
    }
    
    # Map over the chunks to retrieve and process NWIS data
    gage_sites <- purrr::map_dfr(site_chunks, function(chunk) {
      dataRetrieval::whatNWISdata(siteNumber = chunk) %>%
        sf::st_as_sf(coords = c('dec_long_va', 'dec_lat_va'), crs = 4269) %>%
        dplyr::left_join(pcodes, by = "parm_cd") %>%
        dplyr::select(c(site_no,
                        site_name = station_nm,
                        data_type_cd,
                        site_type_cd = site_tp_cd,
                        n_obs = count_nu,
                        begin_date,
                        end_date,
                        #parameter = parameter_name_description,
                        parameter = parameter_nm,
                        code = parm_cd))
    }) %>%
      dplyr::left_join(., table, by = 'site_type_cd') %>%
      dplyr::mutate(data_type = dplyr::case_when(
        data_type_cd == "dv" ~ "Daily",
        data_type_cd == "uv" ~ "Unit",
        data_type_cd == "qw" ~ "Water Quality",
        data_type_cd == "gw" ~ "Groundwater Levels",
        data_type_cd == "iv" ~ "Unit",
        data_type_cd == "sv" ~ "Site Visits",
        data_type_cd == "pk" ~ "Peak Measurements",
        data_type_cd == "ad" ~ "USGS Annual Water Data Report",
        data_type_cd == "aw" ~ "Active Groundwater Level Network",
        data_type_cd == "id" ~ "Historic Instantaneous"))
    
    # Grab NWIS sites by state code: 
  } else if(!is.null(states)){
    
    sites <- dataRetrieval::whatNWISsites(stateCd = states) %>% .$site_no
    
    # Check and split 'sites' into chunks if necessary
    site_chunks <- if (length(sites) > 35000) {
      split(sites, ceiling(seq_along(sites) / 35000))
    } else {
      list(sites)
    }
    
    # Map over the chunks to retrieve and process NWIS data
    gage_sites <- purrr::map_dfr(site_chunks, function(chunk) {
      dataRetrieval::whatNWISdata(siteNumber = chunk) %>%
        sf::st_as_sf(coords = c('dec_long_va', 'dec_lat_va'), crs = 4269) %>%
        dplyr::left_join(pcodes, by = "parm_cd") %>%
        dplyr::select(
          c(
            site_no,
            site_name = station_nm,
            data_type_cd,
            site_type_cd = site_tp_cd,
            n_obs = count_nu,
            begin_date,
            end_date,
            #parameter = parameter_name_description,
            parameter = parameter_nm,
            code = parm_cd
          )
        )
    }) %>%
      dplyr::left_join(., table, by = 'site_type_cd') %>%
      dplyr::mutate(
        data_type = dplyr::case_when(
          data_type_cd == "dv" ~ "Daily",
          data_type_cd == "uv" ~ "Unit",
          data_type_cd == "qw" ~ "Water Quality",
          data_type_cd == "gw" ~ "Groundwater Levels",
          data_type_cd == "iv" ~ "Unit",
          data_type_cd == "sv" ~ "Site Visits",
          data_type_cd == "pk" ~ "Peak Measurements",
          data_type_cd == "ad" ~ "USGS Annual Water Data Report",
          data_type_cd == "aw" ~ "Active Groundwater Level Network",
          data_type_cd == "id" ~ "Historic Instantaneous"
        )
      )
    
  } else {stop("BROKEN")}
  
}
