#' @title Water use tables of a Utah public water system
#'
#' @description
#' Reads the public water system page of the Utah Division of Water Rights
#' (`https://www.waterrights.utah.gov/asp_apps/viewEditPWS/pwsView.asp?SYSTEM_ID=...`)
#' and returns its water use tables, so it needs internet. Only the page's
#' tables with 15 columns (the yearly use tables) are kept.
#'
#' @param WRID String or integer. The system ID of the public water system
#'   (`SYSTEM_ID` in the page address), e.g. from the service areas of
#'   [get_water_suppliers_utah()]. Required.
#'
#' @return A list of tibbles, one per water use table on the page. Empty if
#'   the page has none.
#' @export
#' @seealso [get_water_suppliers_utah()] for the service areas and their IDs.
#' @examplesIf interactive()
#' # Reads pages from the Utah Geospatial Resource Center and the Division of
#' # Water Rights, so it needs internet
#' zion <- npswavacore::get_park_boundary("ZION")
#' suppliers <- get_water_suppliers_utah(zion, dist = 5000)
#' use <- get_water_use_utah(suppliers$WRID[1])
#' length(use)

get_water_use_utah <- function(WRID){

url <- paste0("https://www.waterrights.utah.gov/asp_apps/viewEditPWS/pwsView.asp?SYSTEM_ID=", WRID)

tables <- url |>
  rvest::read_html() |>
  rvest::html_nodes('table')

tables_nes <- vector("list", length = length(tables))

for(i in 1:length(tables)){
  
  try(tables_nes[[i]] <- tables[[i]] |>
        rvest::html_table()
  )
}

tables_tes <- tables_nes |>
  vctrs::list_drop_empty() |>
  purrr::keep(~ncol(.x) == 15)

return(tables_tes)

}
