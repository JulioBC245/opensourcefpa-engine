# SPDX-License-Identifier: AGPL-3.0-or-later
# Copyright (C) 2026 Julio Bauer Cantuaria
# tests/helpers.R — shared bootstrap for gate tests
# LAST CHANGE: 2026-09-18

fpa_test_root <- function() {
  wd <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)
  if (dir.exists(file.path(wd, "R")) && dir.exists(file.path(wd, "tests"))) return(wd)
  if (basename(wd) == "tests") return(normalizePath(dirname(wd), winslash = "/"))
  "."
}

fpa_test_bootstrap <- function() {
  root <- fpa_test_root()
  setwd(root)
  source(file.path(root, "R", "fpa-paths.R"), encoding = "UTF-8")
  source(file.path(root, "R", "fpa-config.R"), encoding = "UTF-8")
  fpa_source_all(root)
  root
}

fpa_fixture_config <- function(root = fpa_test_root()) {
  file.path(root, "data", "fixtures", "config", "fpa-config.json")
}

fpa_ensure_fixtures <- function(root = fpa_test_root()) {
  cfg <- fpa_fixture_config(root)
  if (!file.exists(cfg)) {
    source(file.path(root, "data", "fixtures", "build-fixtures.R"), encoding = "UTF-8")
  }
  # Refresh absolute paths inside config for this machine
  source(file.path(root, "data", "fixtures", "build-fixtures.R"), encoding = "UTF-8")
  invisible(fpa_fixture_config(root))
}


# Optional private datasets, not shipped in this repository.
# Local development may keep them as sibling folders:
#   ../fpa-local-test
#   ../fpa-hotel-alpes
# Override with FPA_OPTIONAL_DATA (a directory) or, for the local-test
# dataset, FPA_CONFIG (a config file inside that tree).
fpa_optional_data_dir <- function(dataset = c("local-test", "hotel-alpes")) {
  dataset <- match.arg(dataset)
  env <- Sys.getenv("FPA_OPTIONAL_DATA", unset = "")
  if (nzchar(env) && dir.exists(env)) {
    return(normalizePath(env, winslash = "/", mustWork = TRUE))
  }
  if (identical(dataset, "local-test")) {
    cfg <- Sys.getenv("FPA_CONFIG", unset = "")
    if (nzchar(cfg) && file.exists(cfg)) {
      return(normalizePath(dirname(cfg), winslash = "/", mustWork = TRUE))
    }
  }
  name <- if (identical(dataset, "hotel-alpes")) "fpa-hotel-alpes" else "fpa-local-test"
  sibling <- file.path(fpa_test_root(), "..", name)
  if (dir.exists(sibling)) {
    return(normalizePath(sibling, winslash = "/", mustWork = TRUE))
  }
  NULL
}

fpa_optional_data_config <- function() {
  cfg <- Sys.getenv("FPA_CONFIG", unset = "")
  if (nzchar(cfg) && file.exists(cfg)) {
    return(normalizePath(cfg, winslash = "/", mustWork = TRUE))
  }
  root <- fpa_optional_data_dir("local-test")
  if (is.null(root)) return(NULL)
  path <- file.path(root, "fpa-config.json")
  if (file.exists(path)) {
    normalizePath(path, winslash = "/", mustWork = TRUE)
  } else {
    NULL
  }
}

fpa_optional_data_message <- function(dataset = "local-test") {
  loc <- if (identical(dataset, "hotel-alpes")) "../fpa-hotel-alpes" else "../fpa-local-test"
  paste0(
    "Optional private test data not found at ", loc,
    " (override with FPA_OPTIONAL_DATA",
    if (!identical(dataset, "hotel-alpes")) " or FPA_CONFIG" else "",
    ")."
  )
}

fpa_notify_skip_optional_data <- function(what, dataset = "local-test") {
  message("SKIP: ", fpa_optional_data_message(dataset), " Skipping ", what, ".")
  invisible(NULL)
}

# Skip cleanly when the optional private dataset is absent.
# Under testthat, uses testthat::skip(). When sourced from tests/run-all.R,
# signals class "fpa_skip" so the runner can record a skip. When launched
# as Rscript, exits 0 after a clear message.
fpa_skip_if_no_optional_data <- function(what = "this check", dataset = "local-test") {
  root <- fpa_optional_data_dir(dataset)
  if (!is.null(root)) return(invisible(root))
  msg <- paste(fpa_optional_data_message(dataset), "Skipping", what)
  if (requireNamespace("testthat", quietly = TRUE)) {
    testing <- isTRUE(tryCatch(testthat::is_testing(), error = function(e) FALSE))
    if (testing) testthat::skip(msg)
  }
  if (isTRUE(getOption("fpa.catch_skips", FALSE))) {
    stop(structure(list(message = msg, call = NULL), class = c("fpa_skip", "error", "condition")))
  }
  message("SKIP: ", msg)
  cat("SKIP optional private test data\n")
  if (!interactive()) quit(save = "no", status = 0L)
  invisible(NULL)
}
