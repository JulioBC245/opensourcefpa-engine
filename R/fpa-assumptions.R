# SPDX-License-Identifier: AGPL-3.0-or-later
# Copyright (C) 2026 Julio Bauer Cantuaria
# fpa-assumptions.R — parametres.csv (periode, parametre, valeur), dotted ids
# LAST CHANGE: 2026-10-08 — assumptions_table clock, latest write wins

#' ISO local time with a colon in the offset, e.g. 2026-10-08T15:35:00+02:00.
fpa_assumptions_stamp_now <- function(when = Sys.time()) {
  when <- as.POSIXct(when)
  off <- format(when, "%z")
  if (nchar(off) == 5L) off <- paste0(substr(off, 1L, 3L), ":", substr(off, 4L, 5L))
  paste0(format(when, "%Y-%m-%dT%H:%M:%S"), off)
}

fpa_assumptions_parse_stamp <- function(stamp) {
  stamp <- trimws(as.character(stamp %||% ""))
  if (!length(stamp) || !nzchar(stamp[[1]])) return(as.POSIXct(NA))
  stamp <- sub("([+-][0-9]{2}):([0-9]{2})$", "\\1\\2", stamp[[1]])
  stamp <- sub("T", " ", stamp, fixed = TRUE)
  parsed <- tryCatch(
    as.POSIXct(stamp, format = "%Y-%m-%d %H:%M:%S%z", tz = "UTC"),
    error = function(e) as.POSIXct(NA)
  )
  if (!length(parsed) || is.na(parsed)) return(as.POSIXct(NA))
  parsed
}

#' Declared parametres.csv. A missing file stays missing: the engine sample is not a stand-in.
fpa_assumptions_csv_path <- function(config, paths = fpa_paths(config)) {
  raw <- config$paths$assumptions %||% ""
  raw <- as.character(raw)
  if (!length(raw) || !nzchar(raw[[1]])) {
    return(file.path(paths$input %||% file.path(paths$working %||% ".", "input"), "parametres.csv"))
  }
  p <- raw[[1]]
  if (file.exists(p)) return(normalizePath(p, winslash = "/", mustWork = FALSE))
  rel <- file.path(paths$working %||% ".", p)
  if (file.exists(rel)) return(normalizePath(rel, winslash = "/", mustWork = FALSE))
  if (!grepl("^[A-Za-z]:[/\\\\]", p) && !startsWith(p, "/") && !startsWith(p, "\\\\")) {
    return(file.path(paths$input %||% ".", basename(p)))
  }
  gsub("\\\\", "/", p)
}

#' Clock of the CSV. The comment wins. Without it, the file modification time is the clock.
fpa_assumptions_csv_clock <- function(path) {
  if (!nzchar(path %||% "") || !file.exists(path)) {
    return(list(stamp = "", at = as.POSIXct(NA), from_comment = FALSE))
  }
  line <- tryCatch(readLines(path, n = 1L, warn = FALSE, encoding = "UTF-8"), error = function(e) "")
  if (length(line) && grepl("^#\\s*written_at\\s*:", line[[1]])) {
    stamp <- trimws(sub("^#\\s*written_at\\s*:\\s*", "", line[[1]]))
    return(list(stamp = stamp, at = fpa_assumptions_parse_stamp(stamp), from_comment = TRUE))
  }
  mt <- file.info(path)$mtime
  stamp <- fpa_assumptions_stamp_now(mt)
  list(stamp = stamp, at = as.POSIXct(mt), from_comment = FALSE)
}

fpa_assumptions_number <- function(raw) {
  if (is.null(raw) || !length(raw) || (length(raw) == 1L && is.na(raw))) return(NA_real_)
  txt <- trimws(as.character(raw[[1]]))
  if (!nzchar(txt)) return(NA_real_)
  suppressWarnings(as.numeric(gsub(",", ".", txt, fixed = TRUE)))
}

#' Numeric rows only. A blank valeur is not a rate.
fpa_assumptions_rows_keep <- function(rows) {
  if (is.null(rows) || !length(rows)) return(list())
  if (is.data.frame(rows)) {
    rows <- lapply(seq_len(nrow(rows)), function(i) as.list(rows[i, , drop = FALSE]))
  } else if (!is.null(rows$periode) || !is.null(rows$parametre)) {
    rows <- list(rows)
  }
  out <- list()
  for (r in rows) {
    if (is.null(r)) next
    per <- trimws(as.character(r$periode %||% ""))
    par <- trimws(as.character(r$parametre %||% ""))
    num <- fpa_assumptions_number(r$valeur)
    if (!nzchar(per) || !nzchar(par) || !is.finite(num)) next
    out[[length(out) + 1L]] <- list(periode = per, parametre = par, valeur = num)
  }
  out
}

fpa_assumptions_row_key <- function(r) paste(r$periode, r$parametre, sep = "\t")

#' JSON numbers replace the same month and id. A blank JSON cell leaves the CSV number.
fpa_assumptions_merge_rows <- function(base, over) {
  out <- fpa_assumptions_rows_keep(base)
  index <- setNames(seq_along(out), vapply(out, fpa_assumptions_row_key, character(1)))
  for (r in fpa_assumptions_rows_keep(over)) {
    key <- fpa_assumptions_row_key(r)
    if (key %in% names(index)) out[[index[[key]]]] <- r else {
      out[[length(out) + 1L]] <- r
      index[[key]] <- length(out)
    }
  }
  out
}

fpa_assumptions_read_lines <- function(path) {
  if (!file.exists(path)) return(list(ok = FALSE, rows = list(), error = "missing"))
  lines <- tryCatch(readLines(path, warn = FALSE, encoding = "UTF-8"), error = function(e) NULL)
  if (is.null(lines)) return(list(ok = FALSE, rows = list(), error = "read_fail"))
  if (length(lines) && grepl("^#\\s*written_at\\s*:", lines[[1]])) lines <- lines[-1L]
  lines <- lines[nzchar(trimws(lines))]
  if (!length(lines)) return(list(ok = TRUE, rows = list(), error = NULL))
  chars <- strsplit(lines[[1]], "", fixed = TRUE)[[1]]
  sep <- if (sum(chars == ";") > sum(chars == ",")) ";" else ","
  df <- tryCatch(
    utils::read.csv(text = paste(lines, collapse = "\n"), sep = sep, stringsAsFactors = FALSE, check.names = FALSE),
    error = function(e) NULL
  )
  if (is.null(df)) return(list(ok = FALSE, rows = list(), error = "read_fail"))
  names(df) <- tolower(trimws(names(df)))
  need <- c("periode", "parametre", "valeur")
  if (!all(need %in% names(df))) {
    return(list(ok = FALSE, rows = list(), error = "schema", data = df))
  }
  list(ok = TRUE, rows = fpa_assumptions_rows_keep(df), error = NULL, data = df)
}

fpa_assumptions_write_csv <- function(path, rows, stamp = "") {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  rows <- fpa_assumptions_rows_keep(rows)
  if (!length(rows)) {
    head <- if (nzchar(stamp %||% "")) paste0("# written_at: ", stamp) else NULL
    writeLines(c(head, "\"periode\",\"parametre\",\"valeur\""), path, useBytes = FALSE)
    return(invisible(path))
  }
  body <- if (length(rows)) {
    data.frame(
      periode = vapply(rows, function(r) r$periode, character(1)),
      parametre = vapply(rows, function(r) r$parametre, character(1)),
      valeur = vapply(rows, function(r) as.numeric(r$valeur), numeric(1)),
      stringsAsFactors = FALSE
    )
  } else {
    data.frame(periode = character(), parametre = character(), valeur = character(), stringsAsFactors = FALSE)
  }
  tmp <- tempfile(fileext = ".csv")
  utils::write.csv(body, tmp, row.names = FALSE, na = "")
  csv_lines <- readLines(tmp, warn = FALSE, encoding = "UTF-8")
  unlink(tmp)
  head <- if (nzchar(stamp %||% "")) paste0("# written_at: ", stamp) else NULL
  writeLines(c(head, csv_lines), path, useBytes = FALSE)
  invisible(path)
}

fpa_assumptions_json_rows <- function(config) {
  block <- config$assumptions_table
  if (is.null(block) || !length(block)) return(list())
  fpa_assumptions_rows_keep(block$rows %||% list())
}

fpa_assumptions_json_clock <- function(config) {
  stamp <- ""
  block <- config$assumptions_table
  if (!is.null(block)) stamp <- trimws(as.character(block$written_at %||% ""))
  if (!length(stamp)) stamp <- ""
  list(stamp = stamp[[1]] %||% "", at = fpa_assumptions_parse_stamp(stamp[[1]] %||% ""))
}

#' Write assumptions_table into the local config file. Nothing is uploaded.
fpa_assumptions_write_json <- function(config_path, rows, stamp) {
  if (!nzchar(config_path %||% "") || !file.exists(config_path)) return(invisible(FALSE))
  cfg <- fpa_load_config(config_path)
  cfg$`.config_path` <- NULL
  cfg$`.engine_version` <- NULL
  cfg$assumptions_table <- list(
    written_at = stamp,
    rows = lapply(fpa_assumptions_rows_keep(rows), function(r) {
      list(periode = r$periode, parametre = r$parametre, valeur = r$valeur)
    })
  )
  jsonlite::write_json(cfg, config_path, auto_unbox = TRUE, pretty = TRUE, null = "null")
  invisible(TRUE)
}

#' One set of rates before the forecast. The later stamp wins. created_at is ignored.
fpa_assumptions_reconcile <- function(config, paths = fpa_paths(config), config_path = config$`.config_path` %||% "") {
  path <- fpa_assumptions_csv_path(config, paths)
  csv_exists <- file.exists(path)
  got <- if (csv_exists) fpa_assumptions_read_lines(path) else list(ok = TRUE, rows = list(), error = "missing")
  csv_rows <- if (isTRUE(got$ok)) got$rows else list()
  json_rows <- fpa_assumptions_json_rows(config)
  csv_filled <- length(csv_rows) > 0L
  json_filled <- length(json_rows) > 0L
  csv_clock <- if (csv_exists) fpa_assumptions_csv_clock(path) else list(stamp = "", at = as.POSIXct(NA), from_comment = FALSE)
  json_clock <- fpa_assumptions_json_clock(config)
  same_instant <- function(a, b) {
    is.finite(as.numeric(a)) && is.finite(as.numeric(b)) && abs(as.numeric(a) - as.numeric(b)) < 1
  }
  if (!csv_filled && !json_filled) {
    if (!csv_exists) fpa_assumptions_write_csv(path, list(), stamp = "")
    return(list(ok = TRUE, action = if (csv_exists) "header_kept" else "header", path = path))
  }
  if (json_filled && !csv_filled) {
    stamp <- if (nzchar(json_clock$stamp)) json_clock$stamp else fpa_assumptions_stamp_now()
    fpa_assumptions_write_csv(path, json_rows, stamp)
    if (!nzchar(json_clock$stamp)) fpa_assumptions_write_json(config_path, json_rows, stamp)
    return(list(ok = TRUE, action = "json_to_csv", path = path, stamp = stamp))
  }
  if (csv_filled && !json_filled) {
    stamp <- if (nzchar(csv_clock$stamp)) csv_clock$stamp else fpa_assumptions_stamp_now()
    fpa_assumptions_write_json(config_path, csv_rows, stamp)
    if (!isTRUE(csv_clock$from_comment)) fpa_assumptions_write_csv(path, csv_rows, stamp)
    return(list(ok = TRUE, action = "csv_to_json", path = path, stamp = stamp))
  }
  json_wins <- is.finite(as.numeric(json_clock$at)) &&
    (!is.finite(as.numeric(csv_clock$at)) || json_clock$at > csv_clock$at + 1)
  if (json_wins) {
    merged <- fpa_assumptions_merge_rows(csv_rows, json_rows)
    stamp <- json_clock$stamp
    if (same_instant(json_clock$at, csv_clock$at) && isTRUE(csv_clock$from_comment)) {
      return(list(ok = TRUE, action = "equal", path = path, stamp = stamp))
    }
    fpa_assumptions_write_csv(path, merged, stamp)
    fpa_assumptions_write_json(config_path, merged, stamp)
    return(list(ok = TRUE, action = "json_wins", path = path, stamp = stamp))
  }
  stamp <- if (nzchar(csv_clock$stamp)) csv_clock$stamp else fpa_assumptions_stamp_now()
  if (isTRUE(csv_clock$from_comment) && same_instant(csv_clock$at, json_clock$at)) {
    return(list(ok = TRUE, action = "equal", path = path, stamp = stamp))
  }
  fpa_assumptions_write_json(config_path, csv_rows, stamp)
  if (!isTRUE(csv_clock$from_comment)) fpa_assumptions_write_csv(path, csv_rows, stamp)
  list(ok = TRUE, action = "csv_wins", path = path, stamp = stamp)
}

fpa_assumptions_reconcile_path <- function(config_path) {
  if (!nzchar(config_path %||% "") || !file.exists(config_path)) {
    return(list(ok = FALSE, action = "missing"))
  }
  config <- fpa_load_config(config_path)
  paths <- fpa_paths(config)
  config <- fpa_apply_local_overlay(config, paths)
  fpa_assumptions_reconcile(config, paths, config_path)
}

#' A screen edit: both clocks become now. The local JSON is not uploaded.
fpa_assumptions_save_edit <- function(config_path, rows) {
  config <- fpa_load_config(config_path)
  paths <- fpa_paths(config)
  config <- fpa_apply_local_overlay(config, paths)
  path <- fpa_assumptions_csv_path(config, paths)
  rows <- fpa_assumptions_rows_keep(rows)
  stamp <- fpa_assumptions_stamp_now()
  fpa_assumptions_write_csv(path, rows, stamp)
  fpa_assumptions_write_json(config_path, rows, stamp)
  list(ok = TRUE, path = path, stamp = stamp, n = length(rows))
}

#' Read assumptions table. Keep only enabled ids. Carry last known rate forward.
#' The first-line clock comment is skipped.
fpa_read_assumptions <- function(config, paths = fpa_paths(config)) {
  path <- fpa_resolve_path(config, "assumptions", paths)
  enabled <- fpa_enabled_assumption_ids(config)
  if (is.na(path) || !nzchar(path) || !file.exists(path)) {
    return(list(ok = FALSE, path = path, data = NULL, wide = NULL, error = "missing"))
  }
  got <- fpa_assumptions_read_lines(path)
  if (!isTRUE(got$ok)) {
    return(list(
      ok = FALSE, path = path, data = NULL, wide = NULL, error = got$error %||% "read_fail",
      message = if (identical(got$error, "schema")) "parametres.csv doit avoir les colonnes periode, parametre, valeur." else ""
    ))
  }
  df <- got$data
  if (is.null(df) || !nrow(df)) {
    df <- data.frame(periode = character(), parametre = character(), valeur = numeric(), stringsAsFactors = FALSE)
  }
  names(df) <- tolower(trimws(names(df)))
  df$parametre <- as.character(df$parametre)
  df$valeur <- suppressWarnings(as.numeric(gsub(",", ".", as.character(df$valeur), fixed = TRUE)))
  df$periode <- as.character(df$periode)
  if (length(enabled)) {
    df <- df[df$parametre %in% enabled, , drop = FALSE]
  }
  wide <- fpa_assumptions_wide(df)
  list(ok = TRUE, path = path, data = df, wide = wide, error = NULL)
}

#' Long -> wide matrix by periode x parametre, with carry-forward per parametre.
fpa_assumptions_wide <- function(df) {
  if (is.null(df) || !nrow(df)) {
    return(data.frame(periode = character(), stringsAsFactors = FALSE))
  }
  periods <- sort(unique(df$periode))
  params <- sort(unique(df$parametre))
  out <- data.frame(periode = periods, stringsAsFactors = FALSE)
  for (p in params) {
    vals <- rep(NA_real_, length(periods))
    last <- NA_real_
    for (i in seq_along(periods)) {
      hit <- df$valeur[df$periode == periods[[i]] & df$parametre == p]
      if (length(hit) && !is.na(hit[[1]])) {
        last <- hit[[1]]
      }
      vals[[i]] <- last
    }
    out[[p]] <- vals
  }
  out
}

#' Numeric assumption for a period (YYYY-MM) and id, with default.
fpa_assumption_num <- function(wide, periode, id, default = NULL) {
  if (is.null(wide) || !nrow(wide) || !(id %in% names(wide))) {
    if (is.null(default)) stop("Parametre manquant: ", id)
    return(default)
  }
  row <- wide[wide$periode == periode, , drop = FALSE]
  if (!nrow(row)) {
    # nearest previous
    earlier <- wide[wide$periode <= periode, , drop = FALSE]
    if (!nrow(earlier)) {
      if (is.null(default)) stop(fpa_txt("loc_param_missing"), periode, ": ", id)
      return(default)
    }
    row <- earlier[nrow(earlier), , drop = FALSE]
  }
  v <- row[[id]][[1]]
  if (is.na(v)) {
    if (is.null(default)) stop("Parametre vide: ", id, " @ ", periode)
    return(default)
  }
  as.numeric(v)
}
