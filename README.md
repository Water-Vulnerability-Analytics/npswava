# npswava

Data retrieval and analysis functions used by the park-specific deep-dive
water supply vulnerability assessments of National Park Service (NPS) units,
part of Water Vulnerability Analytics (WaVA). Every function has a help page,
e.g. `?list_nwis`.

This package was called `wavadeepdive` until November 2026.

## What is in it

| Function family | What it does |
| --- | --- |
| `get_pod_*()`, `get_water_rights_montana()`, `get_water_suppliers_utah()`, `get_water_use_utah()` | State water rights, points of diversion and water suppliers in an area (CA, CO, MT, NM, NV, UT) |
| `list_nwis()`, `add_gw_meta()`, `nldi_finder()`, `baseflow_eckhardt()` | USGS gages and wells, their basins and details, and baseflow separation |
| `get_xy_watersheds()`, `map_nhdplus_hr()`, `get_aoi_source()` | Watersheds, flowlines and the area each water source draws from |
| `get_rockd()`, `get_rockd_aoi()`, `get_formation_info()`, `get_surficial_geology()`, `aquifer_volume()` | Geology (Macrostrat, USGS) and aquifer storage |
| `get_noaa()`, `get_snotel()`, `get_climate_historic()`, `get_climate_future()`, `select_cfs_deep_dive()` | Weather stations, GridMET and MACA climate, and climate future selection |
| `calc_snow_drought_season()` | Daily snow drought status across the cool season |
| `delineate_streamlines()`, `extract_mainstem_path()`, `extract_stream_profile()` | Stream networks from a DEM (needs WhiteboxTools), the main channel, and its longitudinal profile |
| `get_overnight_stays()`, `get_buildings()` | Park visitor use and buildings |
| `project_visitation()`, `linear_trend()`, `annual_growth_rate()`, `gam_logistic()`, `arima_climate()`, `fisichelli()` | Future park visitation, with a choice of methods |
| `calc_monthly_proportions()` | Monthly climatology and each month's share of the annual total |
| `make_ym()`, `calc_water_year()` | Date helpers |

## Install

`npswava` depends on [`npswavacore`](https://github.com/Water-Vulnerability-Analytics/npswavacore)
and on `climateR`, which is installed from GitHub.

```r
# install.packages("remotes")
remotes::install_github("Water-Vulnerability-Analytics/npswava")
```

Some functions read from the shared WaVA data folder, which is not part of this
repository. See `npswavacore::wava_data_path()` for how that folder is found.

`delineate_streamlines()` needs the WhiteboxTools binary
(`whitebox::install_whitebox()`). `arima_climate()` needs the `forecast` package,
which isn't installed with `npswava`.

## AI Use Disclosure

AI-assisted coding tools (Claude Code, using Anthropic's Claude models) were used
during development of this repository, beginning in 2026, to help set up the
package, prepare it for release, and write tests. All AI-assisted code and text
was reviewed, tested, and validated by the authors, who made all design and
scientific decisions and remain responsible for the content.

## License

MIT. See [LICENSE.md](LICENSE.md).
