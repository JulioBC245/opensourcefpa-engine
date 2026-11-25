# SPDX-License-Identifier: AGPL-3.0-or-later
# Copyright (C) 2026 Julio Bauer Cantuaria
# Run gates 1–9. Execute from the opensourcefpa-engine root:
#   "C:\Program Files\R\R-4.6.0\bin\Rscript.exe" tests/run-all.R
# LAST CHANGE: 2026-09-30 (check-*.R scripts run too, one process each)

root <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)
if (!dir.exists(file.path(root, "R"))) {
  root <- "."
  setwd(root)
}

gates <- c(
  sprintf("test-gate-%02d.R", 1:9),
  "test-init.R", "test-liasse-message.R", "test-diagnose.R",
  "test-drivers.R", "test-period-parse.R", "test-phasing.R",
  "test-profile.R", "test-driver-stats.R", "test-driver-grade.R",
  "test-line-lookup.R", "test-open-num.R",
  "test-variance.R", "test-horizon.R", "test-freeze.R", "test-live-hotel.R"
)
options(fpa.catch_skips = TRUE)
failed <- character(0)
for (g in gates) {
  message("=== ", g, " ===")
  code <- tryCatch(
    {
      source(file.path(root, "tests", g), encoding = "UTF-8")
      0L
    },
    fpa_skip = function(e) {
      message("SKIP ", g, ": ", conditionMessage(e))
      0L
    },
    error = function(e) {
      message("FAIL ", g, ": ", conditionMessage(e))
      1L
    }
  )
  if (!identical(code, 0L)) failed <- c(failed, g)
}

# Every check-*.R ends in quit(), so each runs in its own process.
# check-sync-pull.R calls the server and refreshes the live config: set FPA_RUN_SYNC=1 to include it.
if (!exists("%||%")) `%||%` <- function(a, b) if (is.null(a)) b else a
rscript <- file.path(R.home("bin"), if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript")
checks <- sort(list.files(file.path(root, "tests"), pattern = "^check-.*\\.R$"))
if (!identical(Sys.getenv("FPA_RUN_SYNC"), "1")) checks <- setdiff(checks, "check-sync-pull.R")
for (g in checks) {
  message("=== ", g, " ===")
  out <- suppressWarnings(system2(rscript, shQuote(file.path(root, "tests", g)), stdout = TRUE, stderr = TRUE))
  code <- attr(out, "status") %||% 0L
  tail_line <- utils::tail(out[nzchar(out)], 2L)
  message(paste(tail_line, collapse = " | "))
  if (!identical(as.integer(code), 0L)) failed <- c(failed, g)
}

if (length(failed)) {
  stop("Failed: ", paste(failed, collapse = ", "))
}
message("ALL GATES 1-9 + INIT + ", length(checks), " CHECKS PASSED")
