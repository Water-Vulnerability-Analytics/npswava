#' @keywords internal
"_PACKAGE"

## Functions from other packages are called as `pkg::fun()`. The only imports
## are `%>%` (operators can't be called with `::`, see utils-pipe.R) and the
## ggfortify one below.

## ggfortify provides the autoplot() method for PCA objects; importing any of
## its functions loads it.
#' @importFrom ggfortify ggbiplot
NULL

## Lets data.table syntax (`dt[, x := ...]`) work without importing data.table.
.datatable.aware <- TRUE

## Column names used inside dplyr and data.table calls.
utils::globalVariables(c(
   ":=", ".", ".I", ".N", ".SD", "ALLOWNERS", "area",
   "associatedCaseNumbers.x", "associatedCaseNumbers.y", "associatedContacts",
   "b_age", "b_int_age", "b_int_id", "Backcountry", "begin_date",
   "buffer_length2", "catchmentID", "CF", "ConcessionerCamping",
   "ConcessionerLodging", "count_nu", "county.x", "county.y", "CurrInUse",
   "data_type_cd", "DIVERSION_SITE_NAME", "division.x", "division.y",
   "elevation", "end_date", "first_year", "GCM", "geometry", "gnisId.x",
   "gnisId.y", "hx_hy_dist", "hx_ly_dist", "id", "ID", "Label", "LAST_NAME",
   "latitude", "LATITUDE", "locationAccuracy.x", "locationAccuracy.y",
   "longitude", "LONGITUDE", "lx_hy_dist", "lx_ly_dist",
   "MiscellaneousOvernightStays", "Month", "name", "NonRecreationDayUse",
   "NonRecreationOvernightStays", "NonRecreationOvernightUse",
   "NonRecreationVisits", "OBJECTID", "own_lname", "OWNER", "owner_name",
   "parameter_cd", "parameter_nm", "parm_cd", "PC1", "PC2", "pm.x", "pm.y",
   "POD_ID", "point_id", "pr", "range.y", "Rank", "RCP", "RecreationDayUse",
   "RecreationOvernightUse", "RecreationVisits", "Region", "rowid",
   "RVCampers", "SCAN_SNOTEL_metadata", "section.y", "select_cfs", "site_no",
   "site_tp_cd", "source_id", "SOURCE_NAME", "source_table_locs",
   "source_type", "station_nm", "streamMile.x", "streamMile.y",
   "structureName.x", "structureName.y", "structureType.x", "structureType.y",
   "Symbol", "t_age", "t_int_age", "t_int_id", "tasmax", "tasmin",
   "TentCampers", "tmax_C", "tmin_C", "tmmn", "tmmx", "township.y",
   "UNIT_CODE", "UnitCode", "val", "var", "var_temp", "visits",
   "waterDistrict.x", "waterDistrict.y", "waterSource.x", "waterSource.y",
   "weight", "within_aoi", "WRNAME", "x", "X", "x.y", "y", "Y", "y.y", "Year"
))
