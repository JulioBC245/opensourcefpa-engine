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
