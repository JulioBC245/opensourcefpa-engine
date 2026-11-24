# SPDX-License-Identifier: AGPL-3.0-or-later
# Copyright (C) 2026 Julio Bauer Cantuaria
# fpa-init.R — Create the working folder and model tables from fpa-config.json
# LAST CHANGE: 2026-09-18 — replaces the downloadable archive
#
# The website ships one readable text file. Everything the old zip carried
# (folder shape, model CSVs) is rebuilt here from file_drafts / file_schemas.

#' Escape one CSV cell.
fpa_csv_cell <- function(x) {
  s <- if (is.na(x)) "" else as.character(x)
  if (grepl('[",\n]', s)) paste0('"', gsub('"', '""', s, fixed = TRUE), '"') else s
}

#' Build CSV text from a columns/rows draft.
fpa_csv_from_draft <- function(columns, rows) {
  head <- paste(vapply(columns, fpa_csv_cell, character(1)), collapse = ",")
  if (!length(rows)) return(paste0(head, "\n"))
  body <- vapply(rows, function(r) {
    cells <- vapply(seq_along(columns), function(i) {
      v <- if (i <= length(r)) r[[i]] else ""
      fpa_csv_cell(v)
    }, character(1))
    paste(cells, collapse = ",")
  }, character(1))
  paste0(paste(c(head, body), collapse = "\n"), "\n")
}

#' Create working folders and write model tables.
#' @param config_path Path to fpa-config.json
#' @param root Working root override; defaults to config paths$working_root_hint
#' @param overwrite Overwrite existing model files
#' @return Invisible list of created directories and written files
fpa_init <- function(config_path, root = NULL, overwrite = FALSE) {
  config <- fpa_load_config(config_path)
  paths <- fpa_paths(config)
  config <- fpa_apply_local_overlay(config, paths)

  working <- root %||% config$paths$working_root_hint %||% paths$root
  working <- as.character(working)

  dirs <- file.path(working, c("input", "input/modeles", "work", "reports"))
  created <- character(0)
  for (d in dirs) {
    if (!dir.exists(d)) {
      dir.create(d, recursive = TRUE, showWarnings = FALSE)
      created <- c(created, d)
    }
  }

  written <- character(0)
  skipped <- character(0)
  drafts <- config$file_drafts %||% list()
  for (role in names(drafts)) {
    d <- drafts[[role]]
    fname <- as.character(d$filename %||% paste0(role, ".csv"))
    cols <- as.character(unlist(d$columns %||% list()))
    if (!length(cols)) next
    rows <- d$rows %||% list()
    target <- file.path(working, "input", "modeles", fname)
    if (file.exists(target) && !overwrite) {
      skipped <- c(skipped, target)
      next
    }
    txt <- fpa_csv_from_draft(cols, rows)
    con <- file(target, open = "wb")
    writeBin(charToRaw(txt), con)
    close(con)
    written <- c(written, target)
  }

  # Fall back to file_schemas (headers only) for roles without a draft
  schemas <- config$file_schemas %||% list()
  for (role in names(schemas)) {
    if (!is.null(drafts[[role]])) next
    s <- schemas[[role]]
    cols <- as.character(unlist(s$columns %||% list()))
    if (!length(cols)) next
    fname <- gsub("\\.modele", "", as.character(s$filename %||% paste0(role, ".csv")))
    target <- file.path(working, "input", "modeles", fname)
    if (file.exists(target) && !overwrite) {
      skipped <- c(skipped, target)
      next
    }
    txt <- fpa_csv_from_draft(cols, list())
    con <- file(target, open = "wb")
    writeBin(charToRaw(txt), con)
    close(con)
    written <- c(written, target)
  }

  notice <- file.path(working, "input", "modeles", "LISEZMOI.txt")
  if (!file.exists(notice) || overwrite) {
    writeLines(
      c(
        "Modeles CSV generes par le moteur a partir de fpa-config.json.",
        fpa_txt("loc_headers_fixed"),
        fpa_txt("loc_figures_fictional"),
        fpa_txt("loc_save_at_config_path"),
        "",
        fpa_txt("loc_no_program_site")
      ),
      notice,
      useBytes = TRUE
    )
    written <- c(written, notice)
  }

  message(
    "fpa_init: ", length(created), " dossier(s) cree(s), ",
    length(written), fpa_txt("loc_files_written"),
    length(skipped), " conserve(s)."
  )
  invisible(list(
    working = working,
    created = created,
    written = written,
    skipped = skipped
  ))
}
