# Write the golden files for the functions ported from playground PR #165
# (npswava issue #6), from the code in that PR.
#
# Run from the npswava repo root, in a fresh R session (not with npswava
# loaded):
#   Rscript tests/testthat/fixtures/make_golden_pr165.R ../nps_water_vulnerability
#
# make_golden.R can't make these: its baseline, b62f366, is older than these
# functions. This script runs only the cases marked `baseline = "pr165"` in
# golden_cases.R, the same way: it extracts setup.R and src/ from the pinned
# commit below into a temporary folder (the clone is not touched), sources
# setup.R there, and calls the old functions.
#
# It adds its files to golden/ and leaves every other file there alone. They are
# written to a temporary folder first, and copied only if every case ran. The
# inputs are made up, so no case needs the shared data or a download.
#
# Only regenerate these on purpose; never to make a failing test pass.

# The head of playground PR #165 (2026-10-27; not merged yet). The six files are
# byte-identical to the copies in MORA_Paradise_WSVA/src.
golden_commit <- "7bc0211"

args <- commandArgs(trailingOnly = TRUE)
old_repo <- normalizePath(
  if (length(args) > 0) args[1] else "../nps_water_vulnerability",
  mustWork = TRUE
)
fixtures <- normalizePath(file.path("tests", "testthat", "fixtures"),
                          mustWork = TRUE)
golden_dir <- file.path(fixtures, "golden")

if ("npswava" %in% loadedNamespaces()) {
  stop("npswava is loaded; run this in a fresh R session")
}
if (!dir.exists(file.path(old_repo, "data"))) {
  stop("no data folder or link in ", old_repo)
}

git <- function(...) {
  system2("git", c("-C", shQuote(old_repo), ...), stdout = TRUE)
}
golden_commit_full <- git("rev-parse", "--verify", "--quiet",
                          paste0(golden_commit, "^{commit}"))
if (length(golden_commit_full) == 0) {
  stop("commit ", golden_commit, " not found in ", old_repo, "; run ",
       "git fetch upstream pull/165/head")
}
clone_head <- git("rev-parse", "--short", "HEAD")

# the old code exactly as it was at the pinned commit
old_code <- file.path(tempfile("golden_"), "nps_water_vulnerability")
dir.create(old_code, recursive = TRUE)
archive <- tempfile(fileext = ".tar")
git("archive", "--format=tar", "-o", shQuote(archive), golden_commit,
    "setup.R", "src")
utils::untar(archive, exdir = old_code)
file.symlink(normalizePath(file.path(old_repo, "data")),
             file.path(old_code, "data"))

# the new golden files go here until every case has run
out_dir <- tempfile("golden_out_")
dir.create(out_dir)

setwd(old_code)
source("setup.R")

source(file.path(fixtures, "golden_cases.R"))
golden_cases <- Filter(function(case) identical(case$baseline, "pr165"),
                       golden_cases)

info <- c(
  paste("Written:", format(Sys.time(), "%Y-%m-%d %H:%M %Z")),
  paste("Old code: nps_water_vulnerability", golden_commit_full,
        "(head of PR #165; setup.R and src/ extracted from that commit)"),
  paste("Playground clone was on:", clone_head, "(not used for the code)"),
  paste("R:", R.version.string),
  "Package versions: golden_env_pr165.csv",
  ""
)

for (case in golden_cases) {
  message("Running ", case$id)
  warns <- character()
  value <- withCallingHandlers(
    do.call(case$fun, case$args),
    warning = function(w) {
      warns <<- c(warns, conditionMessage(w))
      invokeRestart("muffleWarning")
    },
    error = function(e) {
      message(case$id, " failed, so golden/ was not changed")
    }
  )
  if (!is.null(case$keep)) value <- case$keep(value)
  saveRDS(value, file.path(out_dir, paste0(case$id, ".rds")))
  warn_text <- if (length(warns) > 0) {
    paste0(": ", paste(unique(warns), collapse = " | "))
  } else {
    ""
  }
  info <- c(info, sprintf("%s: %s, %d rows, %d warning(s)%s",
                          case$id, class(value)[1], NROW(value),
                          length(warns), warn_text))
}

writeLines(info, file.path(out_dir, "golden_info_pr165.txt"))

# Recorded after the cases ran, so packages they loaded are included
packages <- sort(loadedNamespaces())
env <- rbind(
  data.frame(item = "R", name = "R", value = as.character(getRversion())),
  data.frame(item = "package", name = packages,
             value = vapply(packages, function(p) {
               as.character(utils::packageVersion(p))
             }, character(1)))
)
utils::write.csv(env, file.path(out_dir, "golden_env_pr165.csv"),
                 row.names = FALSE)

# Every case ran: add the new files, leaving the others in golden/ as they are
dir.create(golden_dir, showWarnings = FALSE)
copied <- file.copy(list.files(out_dir, full.names = TRUE), golden_dir,
                    overwrite = TRUE)
if (!all(copied)) stop("could not copy the golden files into ", golden_dir)
message("Golden files written to ", golden_dir)
