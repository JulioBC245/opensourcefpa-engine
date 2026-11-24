# SPDX-License-Identifier: AGPL-3.0-or-later
# Copyright (C) 2026 Julio Bauer Cantuaria
# fpa-app-entry.R — One call to open the local screen.
# LAST CHANGE: 2026-09-21
# No .bat and no .exe. The subscriber already has an R session open.

fpa_app <- function(config_path = "fpa-config.json") {
  if (!requireNamespace("shiny", quietly = TRUE)) {
    stop(fpa_txt("loc_install_shiny"), call. = FALSE)
  }
  cfg <- normalizePath(config_path, winslash = "/", mustWork = TRUE)
  Sys.setenv(FPA_CONFIG = cfg)
  root <- tryCatch(fpa_detect_root(), error = function(e) dirname(cfg))
  candidates <- c(
    file.path(dirname(cfg), "app.R"),
    file.path(root, "app.R")
  )
  hit <- candidates[file.exists(candidates)]
  if (!length(hit)) {
    stop(
      fpa_txt("loc_app_missing"),
      fpa_txt("loc_copy_app"),
      call. = FALSE
    )
  }
  shiny::runApp(hit[[1]], launch.browser = TRUE)
}
