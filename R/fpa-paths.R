# SPDX-License-Identifier: AGPL-3.0-or-later
# Copyright (C) 2026 Julio Bauer Cantuaria
# fpa-paths.R — Root detection and path list from fpa-config.json
# LAST CHANGE: 2026-09-18 — L1/L2 scaffold

#' Detect the engine root.
#' Accepts the repo layout (R/ + config/) and the subscriber layout
#' (fpa-engine.R + fpa-config.json in the same folder, FPA_HOME set there).
fpa_detect_root <- function() {
  env <- Sys.getenv("FPA_HOME", unset = "")
  if (nzchar(env) && dir.exists(env)) {
    return(normalizePath(env, winslash = "/", mustWork = TRUE))
  }
  wd <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)
  if (file.exists(file.path(wd, "fpa-config.json"))) {
    return(wd)
  }
  if (dir.exists(file.path(wd, "R")) && dir.exists(file.path(wd, "config"))) {
    return(wd)
  }
  if (basename(wd) == "R" && dir.exists(file.path(dirname(wd), "config"))) {
    return(normalizePath(dirname(wd), winslash = "/"))
  }
  if (basename(wd) == "tests" && dir.exists(file.path(dirname(wd), "R"))) {
    return(normalizePath(dirname(wd), winslash = "/"))
  }
  # Climb from sourced file when setwd is elsewhere
  candidates <- c(
    file.path(dirname(wd), "opensourcefpa-engine"),
    file.path(dirname(wd), "fpa-local")
  )
  for (p in candidates) {
    if (dir.exists(p) && dir.exists(file.path(p, "R"))) {
      return(normalizePath(p, winslash = "/", mustWork = TRUE))
    }
  }
  stop(
    "Impossible de trouver la racine opensourcefpa-engine. ",
    "Ouvrez le dossier opensourcefpa-engine (celui qui contient R/ et config/) puis relancez."
  )
}

#' Build a named path list for the engine.
#' @param config Parsed fpa-config list (from fpa_load_config).
#' @param root Project root override.
fpa_paths <- function(config = NULL, root = fpa_detect_root()) {
  root <- normalizePath(root, winslash = "/", mustWork = TRUE)
  working <- root
  if (!is.null(config) && !is.null(config$paths$working_root_hint) &&
      nzchar(as.character(config$paths$working_root_hint))) {
    hint <- as.character(config$paths$working_root_hint)
    # Prefer hint when it exists; otherwise stay under project root for fixtures
    if (dir.exists(hint)) {
      working <- normalizePath(hint, winslash = "/", mustWork = TRUE)
    }
  }
  # Outputs follow the declared working folder, not wherever the engine file
  # happens to sit, so results land next to the inputs the user was told about.
  list(
    root = root,
    r = file.path(root, "R"),
    config_dir = file.path(root, "config"),
    data = file.path(root, "data"),
    fixtures = file.path(root, "data", "fixtures"),
    tests = file.path(root, "tests"),
    work = file.path(working, "work"),
    reports = file.path(working, "reports"),
    exports = file.path(working, "exports"),
    working = working,
    input = file.path(working, "input")
  )
}

#' Apply optional fpa-config.local.json overlay (path corrections on the laptop).
#' Overlay may contain paths.* string replacements and cash_floor.
fpa_apply_local_overlay <- function(config, paths = fpa_paths(config)) {
  local_file <- file.path(paths$config_dir, "fpa-config.local.json")
  # Also check next to the main config if it came from fixtures
  if (!is.null(config$`.config_path`)) {
    alt <- file.path(dirname(config$`.config_path`), "fpa-config.local.json")
    if (file.exists(alt)) local_file <- alt
  }
  if (!file.exists(local_file)) return(config)
  overlay <- jsonlite::fromJSON(local_file, simplifyVector = FALSE)
  if (!is.null(overlay$paths) && length(overlay$paths)) {
    for (nm in names(overlay$paths)) {
      config$paths[[nm]] <- overlay$paths[[nm]]
    }
  }
  if (!is.null(overlay$cash_floor)) {
    v <- suppressWarnings(as.numeric(overlay$cash_floor))
    if (is.finite(v)) config$cash_floor <- v
  }
  config
}

#' Resolve a declared file path from config$paths[[role]].
fpa_resolve_path <- function(config, role, paths = fpa_paths(config)) {
  raw <- config$paths[[role]]
  if (is.null(raw) || !nzchar(as.character(raw))) return(NA_character_)
  p <- as.character(raw)
  # Windows paths in JSON may use backslashes already escaped
  if (file.exists(p)) return(normalizePath(p, winslash = "/", mustWork = FALSE))
  # Relative to working root / fixtures input
  candidates <- c(
    p,
    file.path(paths$working, p),
    file.path(paths$input, basename(p)),
    file.path(paths$fixtures, "input", basename(p))
  )
  for (c in candidates) {
    if (file.exists(c)) return(normalizePath(c, winslash = "/", mustWork = FALSE))
  }
  # Return the declared path even if missing (validate will name it)
  p
}
