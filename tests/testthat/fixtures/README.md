# Golden test fixtures

`test-golden.R` checks that npswava returns exactly what the old code in
`nps_water_vulnerability` returned before the split.

## The baseline: `nps_water_vulnerability` at `b62f366`

We chose one commit of the playground as the reference: `b62f366`, its
`upstream/main` when npswava (then `wavadeepdive`) was split out
(2026-10-20). The golden files always come from that commit, even after the
playground changes.
`make_golden.R` extracts `setup.R` and `src/` from that commit with
`git archive`, whatever the local clone is on. The commit is set in one place,
`golden_commit` at the top of `make_golden.R`.

This is a temporary arrangement for checking the move out of the playground.
Once development has moved off the playground, `b62f366` stops being the
reference, and these tests either retire or become npswava's own
regression baseline.

The setup is copied from npswavacore's golden tests, with two more case fields
(`reads` and `keep`, below).

## Files

- `golden_cases.R`: the calls to compare and the inputs they use. Each case
  has a function, its arguments, and whether it needs the shared data folder
  or internet. Two optional fields:
  - `reads`: for a data case, the file or folder it reads under the data
    folder. The case skips without it, and `make_golden.R` records its
    checksum.
  - `keep`: a function applied to the result before it is saved or compared,
    for results `identical()` can't compare as they are, or to undo a side
    effect. Three cases use it:
    - `select_cfs_deep_dive(to_plot = TRUE)` returns a ggplot, which holds
      environments, so the case keeps the selected models and what the plot
      is built from (data, labels, layer types).
    - `get_climate_historic(return_grid = TRUE)` writes `aoi_gridmet_grid`
      to the global environment, so the case takes it from there, removes it,
      and compares it too.
    - `aquifer_volume(dtw_ft = NA)` turns s2 off and returns without turning
      it back on, so the case turns it back on. Otherwise every later case
      would buffer in degrees.

  The file checks the fields of every case when it is sourced, so a typo
  stops with an error instead of silently dropping a skip.
- `make_golden.R`: runs those calls with the old code at `b62f366` and saves
  the results to `golden/<id>.rds`. Also writes `golden/golden_info.txt`
  (date, the pinned commit, the commit the clone was on, the data folder, row
  and warning counts) and `golden/golden_env.csv` (R and every package
  version, and a checksum of each data file or folder). It writes to a
  temporary folder first and replaces `golden/` only if every case ran, so a
  failed run (e.g. a service is down) leaves the old golden files as they
  were.
- `golden/`: the saved results. Not in git for now; whether the outputs can go
  in this public repo is an open question for the team.

To create the golden files, from the npswava repo root in a fresh R
session (the clone needs commit `b62f366` and its `data` link; the script
doesn't change the clone):

```sh
Rscript tests/testthat/fixtures/make_golden.R ../nps_water_vulnerability
```

It takes about 10 minutes, mostly the Montana water rights geodatabase
(about 430 MB). The checksum reads the whole NHDPlus HR geodatabase for HUC4
1501 (930 MB), so OneDrive downloads all of it the first time.

To run the comparison, point npswava at the same data folder the
playground's `data` link points to:

```sh
WAVA_DATA_DIR="/path/to/shared/data" Rscript -e 'devtools::test(filter = "golden")'
```

Most cases download live data (NWIS, GridMET, MACA, Macrostrat, state water
rights services), and those sources change. Run the comparison soon after
making the golden files. Later failures may come from the source rather than
the package (see below).

The comparison is `expect_identical()`. Every skip says which case it is and
what is missing:

- no golden files at all: one skip for the whole file, saying to run
  `make_golden.R` (this is what `R CMD check` shows, because `golden/` is in
  `.Rbuildignore`);
- one golden file missing: `<case>: golden file missing: <path>`;
- no data: `<case>: no <reads> in the data folder "<path>"`, plus whether the
  path came from the option, `WAVA_DATA_DIR`, or the `"data"` fallback;
- offline: `<case>: offline; this case downloads <what>` (the `downloads`
  field in `golden_cases.R`).

A service that is down while the machine is online is not skipped: the case
errors with the service's error, so an outage is never mistaken for a pass.
That is an error, not a mismatch.

If a test fails, don't regenerate the golden file or loosen the comparison:
find out why the output changed first. A failure can come from npswava,
or from something else that changed since the golden run: an R or package
upgrade, a change to the shared data, or a live source. The failure message
lists the R, package and data changes it finds against `golden_env.csv`, and
names the download if the case has one. To find the cause:

1. Run `make_golden.R` from a scratch copy of npswava (so the real
   `golden/` stays as it is) to rebuild the golden files with today's
   packages, data and downloads.
2. Compare the fresh files with the saved ones (`identical()`).
3. If they are identical, the old code still gives the saved output, so the
   change is in npswava. If they differ, the change came from packages,
   data or a download, not from npswava.

## Not covered

These can't run in either version right now, and a case that errors would
stop `make_golden.R` for every case. Add them once they can run:

- `get_pod_california()`: needs RSelenium, Java and Firefox.
- `get_pod_new_mexico()`: the `OSE_PODs` service returns "Invalid URL" (it was
  renamed `OSE_Points_of_Diversion`).
- `get_pod_nevada()`: its input files (`all/POD_Permits_All.shp`,
  `all/Water_Rights_Points_of_Diversion.csv`) aren't in the shared data
  folder.
- `get_xy_watersheds()` and `get_aoi_source()`: the USGS split-catchment
  service errors (`'NoneType' object has no attribute 'lower'`).
- `get_snotel()`: `soilDB::fetchSCAN()` returns `NULL`.
- `get_rockd_aoi()`: taken out on 2026-11-03. It keeps map units by position
  rather than by source id, so when two sources share a scale its result
  depends on the order Macrostrat returns rows in. With the current order it
  keeps none and queries almost every one of its 10,000 grid points (over an
  hour). Add it back once that is fixed (issue #7). Its default resolution
  (all four values) also errors.
- The partly penetrating well branch of `aquifer_volume()` (it errors:
  `buffer_length2` is undefined).
- The `save = TRUE` branches of `get_noaa()` and `get_snotel()`, which write
  files.
