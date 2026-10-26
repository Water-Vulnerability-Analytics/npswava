# Compare wavadeepdive with golden outputs saved from the old code in
# nps_water_vulnerability at the pinned commit b62f366, the playground when
# wavadeepdive was split out (see fixtures/README.md). A failure means
# wavadeepdive no longer returns exactly what the old code did. Report it;
# don't regenerate the golden file or loosen the comparison to make it pass.
#
# Every skip names the case and what is missing. A service that is down is not
# a skip: its error is reported as is. A failure also says what else has
# changed since the golden run (R, packages, the data it reads, and any live
# download), to help find the cause.

source(test_path("fixtures", "golden_cases.R"), local = TRUE)

golden_dir <- test_path("fixtures", "golden")

# No golden files at all (e.g. under R CMD check, or a fresh clone): one skip
# for the whole file instead of one per case.
if (length(list.files(golden_dir, pattern = "\\.rds$")) == 0) {
  skip(paste0("no golden files in ", golden_dir, "; run ",
              "tests/testthat/fixtures/make_golden.R (see fixtures/README.md)"))
}

data_folder_reason <- function(case) {
  paste0(case$id, ": no ", case$reads, " in the data folder \"",
         wavacore::wava_data_dir(), "\" (", data_dir_source(), ")")
}

# What has changed since the golden files were made, other than wavadeepdive:
# R, package versions and the shared data, against golden_env.csv. Returns a
# data frame with one row per change (`item`: "R", "package" or "data file";
# `name`; `text`), or NULL without golden_env.csv. Shown only when a case
# fails, to point at the cause.
golden_env_changes <- function() {
  env_file <- file.path(golden_dir, "golden_env.csv")
  if (!file.exists(env_file)) return(NULL)
  env <- utils::read.csv(env_file, stringsAsFactors = FALSE)
  now <- vapply(seq_len(nrow(env)), function(i) {
    switch(env$item[i],
      R = as.character(getRversion()),
      package = tryCatch(as.character(utils::packageVersion(env$name[i])),
                         error = function(e) "not installed"),
      "data file" = golden_checksum(wavacore::wava_data_path(env$name[i]))
    )
  }, character(1))
  changed <- env$value != now
  env <- env[changed, ]
  now <- now[changed]
  data.frame(item = env$item, name = env$name, text = ifelse(
    env$item == "data file",
    paste0("data file ", env$name, " (",
           ifelse(now == "missing", "missing", "contents differ"), ")"),
    paste0(env$name, " ", env$value, " -> ", now)
  ))
}

# Worked out on the first failure only, and kept for the rest: the checksums
# read every file the data cases use (the NHDPlus HR geodatabase is 930 MB).
# testthat only evaluates `info` when an expectation fails.
env_cache <- new.env()
env_changes <- function() {
  if (!exists("value", envir = env_cache, inherits = FALSE)) {
    assign("value", golden_env_changes(), envir = env_cache)
  }
  get("value", envir = env_cache, inherits = FALSE)
}

# The text added to a failure: what changed that this case depends on
failure_info <- function(case) {
  all_changes <- env_changes()
  if (is.null(all_changes)) {
    info <- paste("no golden_env.csv, so R, package versions and data files",
                  "can't be compared with the golden run")
  } else {
    changes <- all_changes[all_changes$item != "data file" |
                             all_changes$name %in% case$reads, ]
    info <- if (nrow(changes) == 0) {
      "R, package versions and data files are the same as in the golden run"
    } else {
      paste0("changed since the golden run: ",
             paste(changes$text, collapse = "; "))
    }
  }
  if ("internet" %in% case$needs) {
    info <- paste0(info, ". This case also downloads ", case$downloads,
                   ", which can change on its own")
  }
  info
}

for (case in golden_cases) {
  test_that(paste("golden:", case$id, "matches the old code"), {
    golden_file <- file.path(golden_dir, paste0(case$id, ".rds"))
    skip_if_not(file.exists(golden_file),
                paste0(case$id, ": golden file missing: ", golden_file,
                       "; run tests/testthat/fixtures/make_golden.R"))
    if ("data" %in% case$needs) {
      skip_if_not(file.exists(wavacore::wava_data_path(case$reads)),
                  data_folder_reason(case))
    }
    if ("internet" %in% case$needs) {
      skip_if_not(curl::has_internet(),
                  paste0(case$id, ": offline; this case downloads ",
                         case$downloads))
    }

    actual <- do.call(case$fun, case$args)
    if (!is.null(case$keep)) actual <- case$keep(actual)

    expect_identical(actual, readRDS(golden_file), info = failure_info(case))
  })
}
