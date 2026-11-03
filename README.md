# wavadeepdive

Data retrieval and analysis functions used by the park-specific deep-dive
water supply vulnerability assessments of National Park Service (NPS) units,
part of Water Vulnerability Analytics (WaVA). Every function has a help page,
e.g. `?list_nwis`.

## What is in it

| Function family | What it does |
| --- | --- |
| `get_pod_*()`, `get_water_rights_montana()`, `get_water_suppliers_utah()`, `get_water_use_utah()` | State water rights, points of diversion and water suppliers in an area (CA, CO, MT, NM, NV, UT) |
| `list_nwis()`, `add_gw_meta()`, `nldi_finder()`, `baseflow_eckhardt()` | USGS gages and wells, their basins and details, and baseflow separation |
| `get_xy_watersheds()`, `map_nhdplus_hr()`, `get_aoi_source()` | Watersheds, flowlines and the area each water source draws from |
| `get_rockd()`, `get_rockd_aoi()`, `get_formation_info()`, `get_surficial_geology()`, `aquifer_volume()` | Geology (Macrostrat, USGS) and aquifer storage |
| `get_noaa()`, `get_snotel()`, `get_climate_historic()`, `get_climate_future()`, `select_cfs_deep_dive()` | Weather stations, GridMET and MACA climate, and climate future selection |
| `get_overnight_stays()`, `get_buildings()` | Park visitor use and buildings |
| `make_ym()`, `calc_water_year()` | Date helpers |

## Install

`wavadeepdive` depends on [`wavacore`](https://github.com/Water-Vulnerability-Analytics/wavacore)
and on `climateR`, which is installed from GitHub.

```r
# install.packages("remotes")
remotes::install_github("Water-Vulnerability-Analytics/wavadeepdive")
```

Some functions read from the shared WaVA data folder, which is not part of this
repository. See `wavacore::wava_data_path()` for how that folder is found.

## AI Use Disclosure

AI-assisted coding tools (Claude Code, using Anthropic's Claude models) were used
during development of this repository, beginning in 2026, to help set up the
package, prepare it for release, and write tests. All AI-assisted code and text
was reviewed, tested, and validated by the authors, who made all design and
scientific decisions and remain responsible for the content.

## License

MIT. See [LICENSE.md](LICENSE.md).
