#' @title Area of interest for each water source
#'
#' @description
#' Finds the area each water source draws from, using its `source_type`:
#' * `"Shallow Groundwater"` and `"River/Stream"`: the watershed upstream of
#'   the source, from [getXYWatersheds()]. For wells
#'   (`source_infrastructure == "Well"`) the youngest geologic unit at the
#'   point ([get_rockd()]) is added.
#' * `"Deep Groundwater"`, `"Spring"` and `"Karst Aquifer and Spring"`: the
#'   geologic units at the point ([get_rockd()]). If `aquifer_name` is given,
#'   the formation is looked up with [get_formation_info()] and the units are
#'   narrowed to those whose name matches, or else to those within 5 to 50
#'   million years of its age range. Units larger than 10,000 km2 are
#'   dropped.
#' * Any other type: no area (an empty entry).
#'
#' The source types are matched exactly, in title case. The current WSD from
#' [wavacore::get_wsd()] uses lowercase types (e.g. `"shallow groundwater"`),
#' so convert them first. It needs internet, and makes
#' several requests per source.
#'
#' @param sf sf object. The water sources, as points with `source_type`,
#'   `source_infrastructure` and `aquifer_name` columns. Default:
#'   `source_table_locs`, an object of that name in the calling environment
#'   (as in the deep-dive reports); there is no such object in the package,
#'   so pass the sources.
#'
#' @return `sf` with two columns added: `method` (`"SHALLOW METHOD"`,
#'   `"DEEP METHOD"` or `"OTHER"`) and `aoi`, a list column with one sf
#'   object per source holding its area(s), with `type` (`"Watershed"` or
#'   `"Formation"`) and the watershed or geologic unit columns.
#' @export
#' @seealso [getXYWatersheds()], [get_rockd()] and [get_formation_info()],
#'   which it calls.
#' @examplesIf interactive()
#' # Queries NHDPlus and Macrostrat, so it needs internet
#' src <- sf::st_as_sf(
#'   data.frame(source_type = c("River/Stream", "Deep Groundwater"),
#'              source_infrastructure = c("Surface Intake", "Well"),
#'              aquifer_name = c(NA, "Navajo Sandstone"),
#'              lon = c(-112.9877, -112.95), lat = c(37.2163, 37.25)),
#'   coords = c("lon", "lat"), crs = 4326
#' )
#' src <- get_aoi_source(src)
#' src$method
#' lapply(src$aoi, function(x) unique(x$type))

get_aoi_source <- function(sf = source_table_locs) {
  
  # for testing
 #sf <- source_table_locs[1,]
  
  sf_crs <- sf::st_crs(sf)
  
  
 sf <- sf %>%
      dplyr::mutate(
        method = 
          ifelse(source_type %in% c("Shallow Groundwater", "River/Stream"),
                 "SHALLOW METHOD",  
                 #               ifelse(source_type %in% c("Karst Aquifer and Spring", "Spring"), 
                 #                     "SPRING METHOD", 
                 ifelse(source_type %in% c("Deep Groundwater", 
                                           "Spring", 
                                           "Karst Aquifer and Spring"), 
                        "DEEP METHOD", 
                        "OTHER")))
  
  # Initiate empty aoi vector
  aoi <- vector("list", nrow(sf))  
  
  # Populate each row with aoi
  for(i in 1:nrow(sf)) {
    
   # i <- 2
    
    # Shallow method - get watershed
    if (sf[i,][["source_type"]] %in% c("Shallow Groundwater", "River/Stream")) {
      
      # First get watershed
      ws <- sf[i,]["geometry"] %>% 
        getXYWatersheds(sf = ., coordinates = NULL) %>%
        dplyr::summarize() %>%
        sf::st_transform(., crs = sf_crs) %>%
        dplyr::mutate(type = "Watershed",
                      name = "Watershed") %>%
        sf::st_make_valid()
      aoi[[i]] <- ws
      
      if (sf[i,][["source_infrastructure"]] == "Well") {
        
        # Then get formation extent- shallow so use min age geologic unit as 
        # filter
        all_formations <- sf[i,] %>%
          get_rockd(points = .) %>%
          dplyr::filter(b_int_age == min(b_int_age)) %>%
          dplyr::mutate(type = "Formation") 
        
        if (nrow(all_formations) > 1) {
          
          all_formations <- all_formations[2,]
          
          #combined_unit <- st_union(all_formations)
          
          #combined_unit <- st_sf(geometry = combined_unit, 
          #                       all_formations[1, 
          #                                      !names(all_formations) %in% "geometry"])
          #all_formations <- combined_unit
        }
        
        aoi[[i]] <- dplyr::bind_rows(ws, all_formations)
      }
      
      
      
      # Deep GW method - get intersecting geologic units
    } else if (sf[i,"method"][["method"]] == "DEEP METHOD") {
      
      # extract some info about the formation
      form_type <- stringr::word(sf[i,]$aquifer_name,-1)
      form_name_long <- sf[i,]$aquifer_name
      form_name_short <- stringr::word(form_name_long, 1)
      
      # get all formations intersecting with point
      all_formations <- sf[i,] %>%
        get_rockd(points = .)
      
      # Now, check if metadata available by searching macrostrat using the
      # formation name.  filter all_formations to match metadata
      # found using formation name
      
      # Clear "form_info" variable from previous loop just incase
      if (exists("form_info")) {rm("form_info")}
      
      # Search macrostat database for formation name
      if (!is.na(form_name_short)) {
        form_info <- get_formation_info(gsub(" ","_",form_name_short), form_type) #%>%
        #dplyr::filter(grepl(form_name,Fm)) 
      }
      
      if (exists("form_info")) {
        # Extract age from that info
        form_min_age <- form_info$t_age %>% min(., na.rm = TRUE)
        form_max_age <- form_info$t_age %>% max(., na.rm = TRUE)
        
        # Use formation name to filter all_formations
        select_formation <- all_formations %>%
          dplyr::filter(grepl(form_name_long, name))
        
        # If nothing returned, use formation age to filter all_formations
        if(nrow(select_formation) < 1) {
          start_buffer = 5
          while(nrow(select_formation) <1) {
            select_formation <- all_formations %>%
              dplyr::filter(t_age >= form_min_age-start_buffer & 
                              t_age <= form_max_age+start_buffer)
            start_buffer <- start_buffer + 5
            if (start_buffer > 50) break # break if nothing matched with +/- 50 years
          }
        } 
        
        # If still nothing returned, return all_formations (for now)
        # Will need to investigate further. 
        if (nrow(select_formation) < 1) {
          select_formation <- all_formations
        }
      } else {
        # if no metadata available, then assign select_formation as all formations
        select_formation <- all_formations
      }
      
      select_formation <- select_formation %>%
        sf::st_transform(., sf_crs) %>%
        sf::st_make_valid() %>%
        dplyr::mutate(type = "Formation",
                      area = as.numeric(sf::st_area(sf::st_make_valid(geometry)) %>% units::set_units("km^2"))) %>%
        dplyr::filter(area < 1e4) # remove aoi > 10,000 km2
      
      aoi[[i]] <- select_formation
    }
  }
  
  sf$aoi <- purrr::map(aoi, dplyr::bind_rows)
  
  return(sf)
  
}


# Get formation info - used in get_aoi_source()
#' @title Look up a geologic formation by name (Macrostrat)
#'
#' @description
#' Searches the Macrostrat units API (<https://macrostrat.org/>) for a
#' stratigraphic name, so it needs internet, and returns the matching units
#' as polygons. Used by [get_aoi_source()] to find the formation of a deep
#' groundwater or spring source.
#'
#' @param formation_name String. The stratigraphic name, with underscores
#'   for spaces, e.g. `"Navajo"` or `"Cedar_Mesa"`. Required.
#' @param rock_type String. A lithology group (e.g. `"sandstone"`), meant for
#'   a second search if the name alone finds nothing. In practice the first
#'   search's result is always returned, so this has no effect. Required.
#'
#' @return An sf object of unit polygons (CRS 4326) with the Macrostrat unit
#'   columns (e.g. `unit_id`, `strat_name_long`, `Fm`, `t_age`, `b_age`).
#'   Empty if nothing matches. If the request fails, the function stops with
#'   an error.
#' @export
#' @seealso [get_rockd()] for units at points; [get_aoi_source()].
#' @examplesIf interactive()
#' # Queries the Macrostrat API, so it needs internet
#' navajo <- get_formation_info("Navajo", "sandstone")
#' range(as.numeric(navajo$t_age))

get_formation_info <- function(formation_name, rock_type){
  
  # grab geologic layers that intersect the coordinates of the points of interest
  # y <- RCurl::getURL(paste0("https://macrostrat.org/api/units?strat_name=",formation_name,"&format=json"),
  #                   .opts = RCurl::curlOptions(ssl.verifypeer=FALSE, verbose=TRUE)) %>%
  #  fromJSON()
  
  #table_result <- y$success$data
  
  #https://macrostrat.org/api/units?strat_name=cedar_mesa
  
  if(exists("table_result")){rm("table_result")}
  
  tryCatch({
    # First try formation name search (without rock type)
    table_result <- RCurl::getURL(paste0("https://macrostrat.org/api/units?strat_name=",formation_name,"&format=geojson_bare&s"),
                                  .opts = RCurl::curlOptions(ssl.verifypeer=FALSE, verbose=TRUE)) %>%
      geojsonsf::geojson_wkt(.) %>%
      dplyr::bind_rows() %>%
      sf::st_as_sf(., wkt = 'geometry', crs = 4326)
    # Check if successful (no error)
    if (!is.null(table_result)) return(table_result)
    
    # If first try fails, attempt with rock type (assuming error handling in case both fail)
    table_result <- RCurl::getURL(paste0("https://macrostrat.org/api/units?strat_name=",formation_name,"&lith_group=",rock_type,"&format=geojson_bare"),
                                  .opts = RCurl::curlOptions(ssl.verifypeer=FALSE, verbose=TRUE)) %>%
      geojsonsf::geojson_wkt(.) %>%
      dplyr::bind_rows() %>%
      sf::st_as_sf(., wkt = 'geometry', crs = 4326)
    return(table_result)
  }, error = function(e) {
    # do nothing
    table_result <- NULL
  })
  
  return(table_result)
  
}
