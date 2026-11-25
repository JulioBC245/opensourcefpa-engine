# SPDX-License-Identifier: AGPL-3.0-or-later
# Copyright (C) 2026 Julio Bauer Cantuaria
# fpa-config.R — Load and query fpa-config.json from the website wizard
# LAST CHANGE: 2026-09-21 — fpa_open_num (missing AP/AR/INV on named BS)

FPA_ENGINE_VERSION <- "0.1.0"

#' Compare semver-ish strings (major.minor.patch). Returns TRUE if a >= b.
fpa_version_ge <- function(a, b) {
  pa <- as.integer(strsplit(gsub("[^0-9.]", "", a), "\\.", fixed = FALSE)[[1]])
  pb <- as.integer(strsplit(gsub("[^0-9.]", "", b), "\\.", fixed = FALSE)[[1]])
  n <- max(length(pa), length(pb))
  length(pa) <- n
  length(pb) <- n
  pa[is.na(pa)] <- 0L
  pb[is.na(pb)] <- 0L
  for (i in seq_len(n)) {
    if (pa[i] > pb[i]) return(TRUE)
    if (pa[i] < pb[i]) return(FALSE)
  }
  TRUE
}

#' Load fpa-config.json. Stops with a plain French message on hard failures.
fpa_load_config <- function(config_path) {
  if (!file.exists(config_path)) {
    stop(
      fpa_txt("loc_config_missing"), config_path, ". ",
      fpa_txt("loc_config_place")
    )
  }
  # Notepad and Excel can save UTF-8 with a byte-order mark; jsonlite warns on it.
  bytes <- readBin(config_path, "raw", n = file.info(config_path)$size)
  if (length(bytes) >= 3 &&
      identical(as.integer(bytes[1:3]), c(239L, 187L, 191L))) {
    bytes <- bytes[-seq_len(3)]
  }
  txt <- rawToChar(bytes)
  Encoding(txt) <- "UTF-8"
  raw <- tryCatch(
    jsonlite::fromJSON(txt, simplifyVector = FALSE),
    error = function(e) {
      stop(
        fpa_txt("loc_config_bad_json"),
        basename(config_path), "). Verifiez le telechargement."
      )
    }
  )
  min_v <- raw$engine_min_version %||% "0.1.0"
  if (!fpa_version_ge(FPA_ENGINE_VERSION, min_v)) {
    stop(
      "Ce moteur (version ", FPA_ENGINE_VERSION,
      fpa_txt("loc_config_too_old"), min_v, "). ",
      "Mettez a jour opensourcefpa-engine."
    )
  }
  raw$`.config_path` <- normalizePath(config_path, winslash = "/", mustWork = FALSE)
  raw$`.engine_version` <- FPA_ENGINE_VERSION
  raw
}

`%||%` <- function(a, b) if (is.null(a) || (length(a) == 1 && is.na(a))) b else a

#' Read a key from the opening BS vector without a subscript error.
#' Named numeric vectors throw on a missing name; lists return NULL.
fpa_open_num <- function(opening, name, default = 0) {
  if (is.null(opening) || !length(opening)) return(default)
  nms <- names(opening)
  key <- as.character(name)
  if (is.null(nms) || !length(nms)) return(default)
  if (!(key %in% nms)) {
    hit <- which(toupper(nms) == toupper(key))
    if (!length(hit)) return(default)
    key <- nms[[hit[[1]]]]
  }
  v <- tryCatch(opening[[key]], error = function(e) NULL)
  if (is.null(v) || (length(v) == 1 && is.na(v))) return(default)
  as.numeric(v)[[1]]
}

#' TRUE if mechanism id is enabled in config$mechanisms.
fpa_mechanism_on <- function(config, id) {
  m <- config$mechanisms
  if (is.null(m) || is.null(m[[id]])) return(FALSE)
  isTRUE(m[[id]]) || identical(m[[id]], TRUE) || identical(as.character(m[[id]]), "TRUE")
}

#' Enabled assumption_keys rows (list of lists with id, meaning, engine, origin).
fpa_enabled_assumptions <- function(config) {
  keys <- config$assumption_keys %||% list()
  out <- list()
  for (k in keys) {
    en <- k$enabled
    if (is.null(en) || isTRUE(en) || identical(en, TRUE)) {
      out[[length(out) + 1]] <- k
    }
  }
  out
}

#' Character vector of enabled assumption ids.
fpa_enabled_assumption_ids <- function(config) {
  vapply(fpa_enabled_assumptions(config), function(k) as.character(k$id), character(1))
}

#' Named character vector: user_label -> functional_label for a statement.
#'
#' The budget plans the income statement, so it uses the same functional
#' labels. Its own wording is rarely the ledger's, hence a map of its own.
#' Older configs carry no budget map: fall back to the P&L one.
fpa_line_map <- function(config, statement = c("pnl", "bs", "budget")) {
  statement <- match.arg(statement)
  if (statement == "pnl") {
    m <- config$line_map %||% list()
  } else if (statement == "bs") {
    m <- config$line_map_bs %||% list()
  } else {
    m <- config$line_map_budget %||% list()
    if (!length(m) && !length(config$line_bindings_budget %||% list())) {
      return(fpa_line_map(config, "pnl"))
    }
  }
  if (!length(m)) {
    # Fall back to line_bindings
    binds <- config$line_bindings %||% list()
    m <- list()
    for (b in binds) {
      if (!identical(b$statement, statement)) next
      if (!isTRUE(b$import) && !identical(b$role, "input")) next
      fl <- as.character(b$functional_label %||% "")
      if (!nzchar(fl) || fl %in% c("IGNORE", "SECTION", "SUBTOTAL")) next
      m[[as.character(b$user_label)]] <- fl
    }
  }
  if (!length(m)) return(character(0))
  setNames(as.character(unlist(m)), names(m))
}

#' Source entry for a file_role, or NULL.
fpa_source_for_role <- function(config, role) {
  for (s in config$sources %||% list()) {
    if (identical(s$file_role, role)) return(s)
  }
  NULL
}

#' Source every module named in engine-modules.txt. A missing file stops the load.
fpa_source_all <- function(root = fpa_detect_root()) {
  manifest <- file.path(root, "engine-modules.txt")
  if (!file.exists(manifest)) {
    stop(fpa_txt("loc_module_list_missing"), manifest, call. = FALSE)
  }
  lines <- readLines(manifest, warn = FALSE, encoding = "UTF-8")
  mods <- trimws(sub("#.*$", "", lines))
  mods <- mods[nzchar(mods)]
  if (!length(mods)) stop("engine-modules.txt est vide.", call. = FALSE)
  rdir <- file.path(root, "R")
  for (m in mods) {
    f <- file.path(rdir, m)
    if (!file.exists(f)) stop("Module manquant : ", f, call. = FALSE)
    source(f, local = FALSE, encoding = "UTF-8")
  }
  invisible(mods)
}
