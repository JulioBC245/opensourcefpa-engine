# SPDX-License-Identifier: AGPL-3.0-or-later
# Copyright (C) 2026 Julio Bauer Cantuaria
# fpa-diagnose.R — First-pass check: files on disk vs fpa-config.json
# LAST CHANGE: 2026-09-21
#
# Prints a readable console report. Blocking issues stop the run from
# fpa_main without an R traceback. fpa_run() is unchanged (tests).

FPA_DIAG_WIDTH <- 72L

FPA_DIAG_ROLES <- c(
  pnl_actuals = "Compte de resultat (historique)",
  pnl_monthly = "Compte de resultat mensuel",
  opening_bs = "Bilan d'ouverture",
  budget = "Budget"
)

FPA_DIAG_WIZARD <- c(
  Colonnes = "etape 3  Colonnes & lignes",
  PlanBS = "etape 4.3  Bilan d'ouverture",
  PlanPNL = "etape 4.1  Compte de resultat",
  PlanBudget = "etape 4.4  Budget",
  Chemins = "etape 6  Chemins",
  Fichiers = "etape 2  Fichiers",
  Revue = "etape 7  Revue (telecharger fpa-config.json)"
)

fpa_diag_show_path <- function(p) {
  if (is.null(p) || !nzchar(as.character(p))) return("(chemin vide)")
  normalizePath(as.character(p), winslash = "\\", mustWork = FALSE)
}

fpa_diag_eur <- function(x) {
  paste0(format(round(as.numeric(x)), big.mark = " ", scientific = FALSE), " EUR")
}

fpa_diag_rule <- function(char = "-", n = FPA_DIAG_WIDTH) {
  paste0(strrep(char, n), "\n")
}

#' Wrap one paragraph to the console width.
fpa_diag_wrap <- function(text, indent = "    ", width = FPA_DIAG_WIDTH) {
  text <- paste(as.character(text), collapse = " ")
  text <- gsub("\\s+", " ", trimws(text))
  if (!nzchar(text)) return(character(0))
  room <- max(20L, as.integer(width) - nchar(indent, type = "width"))
  words <- strsplit(text, " ", fixed = TRUE)[[1]]
  lines <- character(0)
  cur <- ""
  for (w in words) {
    trial <- if (!nzchar(cur)) w else paste(cur, w)
    if (nchar(trial, type = "width") <= room) {
      cur <- trial
    } else {
      if (nzchar(cur)) lines <- c(lines, paste0(indent, cur))
      cur <- w
    }
  }
  if (nzchar(cur)) lines <- c(lines, paste0(indent, cur))
  lines
}

fpa_diag_item <- function(role, title, file, path, severity, code,
                          problem, where, step, action) {
  list(
    role = as.character(role),
    title = as.character(title),
    file = as.character(file %||% ""),
    path = as.character(path %||% ""),
    severity = as.character(severity),
    code = as.character(code),
    problem = as.character(problem),
    where = as.character(where),
    step = as.character(step %||% ""),
    action = as.character(action)
  )
}

fpa_diag_is_total_label <- function(label) {
  f <- fpa_fold_header(label)
  if (grepl("total", f) && grepl("actif", f) && !grepl("passif", f)) return("actif")
  if (grepl("total", f) && grepl("passif", f)) return("passif")
  if (grepl("^(total|sous total|sous-total)\\b", f)) return("other")
  ""
}

fpa_diag_libelle_col <- function(df, src) {
  cmap <- src$column_map %||% list()
  for (hdr in names(cmap)) {
    if (identical(as.character(cmap[[hdr]]), "libelle") && hdr %in% names(df)) {
      return(hdr)
    }
  }
  nms <- names(df)
  hit <- nms[grepl("libelle|poste|intitule|label|ligne", nms, ignore.case = TRUE)]
  if (length(hit)) return(hit[[1]])
  for (c in nms) {
    if (is.character(df[[c]]) || is.factor(df[[c]])) return(c)
  }
  nms[[1]]
}

fpa_diag_amount_col <- function(df, src) {
  cmap <- src$column_map %||% list()
  for (hdr in names(cmap)) {
    if (identical(as.character(cmap[[hdr]]), "montant") && hdr %in% names(df)) {
      return(hdr)
    }
  }
  nms <- names(df)
  for (c in nms) {
    if (fpa_header_is_amount(c) && c %in% nms) return(c)
  }
  for (c in nms) {
    if (is.numeric(df[[c]])) return(c)
  }
  NULL
}

fpa_diag_row_amount <- function(df, i, amt_col = NULL) {
  if (!is.null(amt_col) && amt_col %in% names(df)) {
    v <- suppressWarnings(as.numeric(gsub(",", ".", as.character(df[[amt_col]][[i]]))))
    if (length(v) && !is.na(v)) return(v)
  }
  tot <- 0
  n <- 0L
  for (c in names(df)) {
    if (!is.numeric(df[[c]]) && !is.integer(df[[c]])) {
      v <- suppressWarnings(as.numeric(gsub(",", ".", as.character(df[[c]][[i]]))))
    } else {
      v <- as.numeric(df[[c]][[i]])
    }
    if (length(v) && !is.na(v) && is.finite(v)) {
      tot <- tot + v
      n <- n + 1L
    }
  }
  if (!n) return(NA_real_)
  tot
}

fpa_diag_where_line <- function(where) {
  switch(
    where,
    assistant = "ASSISTANT (le JSON), pas Excel",
    excel = fpa_txt("loc_excel_not_setup"),
    poste = "CE POSTE (R / dossier), pas Excel ni l'assistant",
    where
  )
}

fpa_diag_cmap_roles <- function(src) {
  cmap <- src$column_map %||% list()
  if (!length(cmap)) return(character(0))
  setNames(as.character(unlist(cmap)), names(cmap))
}

fpa_diag_has_period_end <- function(src) {
  cmap <- src$column_map %||% list()
  pm <- names(cmap)[as.character(unlist(cmap)) == "periode_montant"]
  if (!length(pm)) return(FALSE)
  colper <- src$column_period %||% list()
  for (h in pm) {
    pe <- colper[[h]]
    if (!is.null(pe) && nzchar(as.character(pe))) return(TRUE)
    if (grepl("20[0-9]{2}", h)) return(TRUE)
  }
  for (p in src$periods %||% list()) {
    pe <- p$period_end
    if (!is.null(pe) && nzchar(as.character(pe))) return(TRUE)
  }
  FALSE
}

fpa_diag_amount_mapped_to_periode <- function(src) {
  cmap <- src$column_map %||% list()
  if (!length(cmap)) return(character(0))
  bad <- character(0)
  for (hdr in names(cmap)) {
    if (identical(as.character(cmap[[hdr]]), "periode") && fpa_header_is_amount(hdr)) {
      bad <- c(bad, hdr)
    }
  }
  bad
}

#' Inspect one declared financial file. Returns a list of diag items (maybe empty).
fpa_diagnose_source <- function(config, role, path, src, title = NULL) {
  items <- list()
  title <- title %||% (FPA_DIAG_ROLES[[role]] %||% role)
  fname <- if (is.null(path) || !nzchar(path)) "" else basename(path)
  push <- function(...) {
    items[[length(items) + 1]] <<- fpa_diag_item(role, title, fname, path, ...)
  }

  if (is.null(path) || !nzchar(as.character(path))) {
    return(items)
  }

  if (!file.exists(path)) {
    push(
      severity = "error", code = "missing_file",
      problem = paste0(
        fpa_txt("loc_file_not_at_json")
      ),
      where = "poste",
      step = "Chemins",
      action = c(
        paste0(fpa_txt("loc_put_file_here"), fpa_diag_show_path(path)),
        fpa_txt("loc_fix_path_step")
      )
    )
    return(items)
  }

  ext <- tolower(tools::file_ext(path))
  if (ext %in% c("xlsx", "xls", "xlsm") &&
      !requireNamespace("readxl", quietly = TRUE)) {
    push(
      severity = "error", code = "readxl_missing",
      problem = fpa_txt("loc_readxl_missing"),
      where = "poste",
      step = "",
      action = fpa_txt("loc_install_readxl")
    )
    return(items)
  }

  if (ext %in% c("xlsx", "xls", "xlsm") &&
      !requireNamespace("openxlsx", quietly = TRUE)) {
    push(
      severity = "warning", code = "openxlsx_missing",
      problem = fpa_txt("loc_openxlsx_missing"),
      where = "poste",
      step = "",
      action = fpa_txt("loc_install_openxlsx")
    )
  }

  raw <- fpa_read_raw_file(path)
  if (!is.null(raw$error)) {
    push(
      severity = "error", code = "unreadable",
      problem = paste0(
        fpa_txt("loc_file_unreadable"),
        raw$message %||% "Format non reconnu."
      ),
      where = "excel",
      step = "",
      action = fpa_txt("loc_resave_utf8")
    )
    return(items)
  }
  df <- raw$data
  src <- src %||% list()
  cmap <- src$column_map %||% list()
  layout <- as.character(src$file_layout %||% "long")

  missing_hdrs <- setdiff(names(cmap), names(df))
  if (length(missing_hdrs)) {
    push(
      severity = "error", code = "header_missing",
      problem = paste0(
        fpa_txt("loc_json_cols_missing"),
        paste(missing_hdrs, collapse = ", "),
        ". Colonnes lues: ",
        paste(names(df), collapse = " | "), "."
      ),
      where = "assistant",
      step = "Colonnes & lignes",
      action = c(
        "Verifiez que l'Excel n'a pas ete renomme depuis le mapping.",
        fpa_txt("loc_rerun_columns")
      )
    )
  }

  bad_periode <- fpa_diag_amount_mapped_to_periode(src)
  looks_liasse <- isTRUE(fpa_looks_like_liasse(df, src))
  if (identical(role, "opening_bs") && !length(bad_periode)) {
    looks_liasse <- FALSE
  }
  roles <- fpa_diag_cmap_roles(src)
  has_pm <- any(roles == "periode_montant")

  if (length(bad_periode) || (looks_liasse && identical(layout, "long") && !has_pm)) {
    cols <- paste(names(df), collapse = " | ")
    mapped <- if (length(cmap)) {
      paste(sprintf("%s -> %s", names(cmap), as.character(unlist(cmap))), collapse = " ; ")
    } else {
      "(aucun)"
    }
    push(
      severity = "error", code = "liasse_as_long",
      problem = c(
        paste0(
          fpa_txt("loc_annual_pnl"),
          fpa_txt("loc_not_month_rows")
        ),
        paste0("Colonnes lues: ", cols, "."),
        paste0("Mapping JSON: ", mapped, "."),
        if (length(bad_periode)) {
          paste0(
            fpa_txt("loc_amount_cols_on_period"),
            paste(bad_periode, collapse = ", "), "."
          )
        } else {
          fpa_txt("loc_json_long_mode")
        }
      ),
      where = "assistant",
      step = "Colonnes & lignes",
      action = c(
        "Ne changez pas l'Excel.",
        fpa_txt("loc_pick_period_amount"),
        fpa_txt("loc_dont_pick_period"),
        fpa_txt("loc_switch_wide")
      )
    )
  } else if (has_pm && !fpa_diag_has_period_end(src)) {
    push(
      severity = "error", code = "wide_no_period_end",
      problem = paste0(
        fpa_txt("loc_period_no_year_end"),
        fpa_txt("loc_period_empty_note")
      ),
      where = "assistant",
      step = "Colonnes & lignes",
      action = c(
        fpa_txt("loc_give_year_end"),
        "Telechargez a nouveau fpa-config.json."
      )
    )
  }

  # Opening BS: Excel totals vs JSON import
  if (identical(role, "opening_bs")) {
    bs_items <- fpa_diagnose_opening_bs(df, src, config, path, title, fname)
    items <- c(items, bs_items)
  }

  items
}

fpa_diagnose_opening_bs <- function(df, src, config, path, title, fname) {
  items <- list()
  push <- function(...) {
    items[[length(items) + 1]] <<- fpa_diag_item(
      "opening_bs", title, fname, path, ...
    )
  }
  lab_col <- fpa_diag_libelle_col(df, src)
  amt_col <- fpa_diag_amount_col(df, src)
  if (is.null(lab_col) || !nrow(df)) return(items)

  line_map <- fpa_line_map(config, "bs")
  excel_actif <- NA_real_
  excel_passif <- NA_real_
  skipped <- list()
  unmapped <- list()
  imported <- numeric(0)

  for (i in seq_len(nrow(df))) {
    lab <- trimws(as.character(df[[lab_col]][[i]]))
    if (!nzchar(lab)) next
    amt <- fpa_diag_row_amount(df, i, amt_col)
    if (is.na(amt)) next
    tot_kind <- fpa_diag_is_total_label(lab)
    if (identical(tot_kind, "actif")) {
      excel_actif <- amt
      next
    }
    if (identical(tot_kind, "passif")) {
      excel_passif <- amt
      next
    }
    fl <- fpa_lookup_line(lab, line_map)
    if (is.na(fl) || !nzchar(fl)) {
      if (abs(amt) > 0 && !identical(tot_kind, "other")) {
        unmapped[[length(unmapped) + 1]] <- list(lab = lab, amt = amt)
      }
      next
    }
    if (fl %in% c("SECTION", "SUBTOTAL", "IGNORE")) {
      if (abs(amt) > 0 && !identical(tot_kind, "other")) {
        skipped[[length(skipped) + 1]] <- list(lab = lab, amt = amt, fl = fl)
      }
      next
    }
    if (!length(imported)) {
      imported <- setNames(amt, fl)
    } else if (fl %in% names(imported)) {
      imported[[fl]] <- imported[[fl]] + amt
    } else {
      imported <- c(imported, setNames(amt, fl))
    }
  }

  excel_ok <- FALSE
  if (!is.na(excel_actif) && !is.na(excel_passif)) {
    excel_ok <- abs(excel_actif - excel_passif) <= 1
    if (!excel_ok) {
      push(
        severity = "error", code = "bs_excel_unbalanced",
        problem = paste0(
          fpa_txt("loc_excel_total_assets"), fpa_diag_eur(excel_actif),
          ") et Total Passif (", fpa_diag_eur(excel_passif),
          fpa_txt("loc_not_equal_gap"),
          fpa_diag_eur(abs(excel_actif - excel_passif)), "."
        ),
        where = "excel",
        step = "",
        action = fpa_txt("loc_fix_bs_file")
      )
      return(items)
    }
  }

  bal <- fpa_bs_balance_check(imported)
  drop_amt <- sum(vapply(skipped, function(s) as.numeric(s$amt), numeric(1)), na.rm = TRUE) +
    sum(vapply(unmapped, function(s) as.numeric(s$amt), numeric(1)), na.rm = TRUE)
  engine_unbal <- !isTRUE(bal$ok)
  empty_import <- !length(imported) && drop_amt > 0
  excel_vs_engine <- excel_ok && abs((bal$assets %||% 0) - excel_actif) > 1
  if ((length(skipped) || length(unmapped)) && (engine_unbal || empty_import || excel_vs_engine)) {
    skip_lines <- vapply(skipped, function(s) {
      sprintf(
        "            - %-28s %12s   JSON: %s (non importe)",
        s$lab, fpa_diag_eur(s$amt), s$fl
      )
    }, character(1))
    um_lines <- vapply(unmapped, function(s) {
      sprintf(
        "            - %-28s %12s   absent du plan",
        s$lab, fpa_diag_eur(s$amt)
      )
    }, character(1))
    excel_line <- if (excel_ok) {
      paste0(
        "Excel balance: Total Actif ", fpa_diag_eur(excel_actif),
        " = Total Passif ", fpa_diag_eur(excel_passif), "."
      )
    } else {
      fpa_txt("loc_excel_totals_unread")
    }
    push(
      severity = "error", code = "bs_map_drops_amounts",
      problem = c(
        excel_line,
        paste0(
          fpa_txt("loc_json_partial_bs"),
          fpa_diag_eur(bal$assets %||% 0),
          " / passif + capitaux: ",
          fpa_diag_eur((bal$liabilities %||% 0) + (bal$equity %||% 0)),
          ". Ecart moteur: ", fpa_diag_eur(abs(bal$gap %||% 0)), "."
        ),
        paste0(fpa_txt("loc_lines_excluded"), fpa_diag_eur(drop_amt), "):"),
        c(skip_lines, um_lines)
      ),
      where = "assistant",
      step = "PlanBS",
      action = c(
        "Ne changez pas l'Excel.",
        fpa_txt("loc_step43_section"),
        fpa_txt("loc_step43_example"),
        "Total Actif / Total Passif restent SUBTOTAL.",
        "Relancez l'assistant sur 4.3 si besoin, puis telechargez fpa-config.json."
      )
    )
  } else if (!isTRUE(bal$ok) && length(imported)) {
    push(
      severity = "error", code = "bs_engine_unbalanced",
      problem = bal$message,
      where = "excel",
      step = "",
      action = fpa_txt("loc_imported_unbalanced")
    )
  }

  items
}

fpa_diagnose_extras <- function(config) {
  items <- list()
  extras <- config$extra_files %||% list()
  if (!length(extras)) return(items)
  for (ex in extras) {
    p <- as.character(ex$path %||% "")
    nm <- as.character(ex$name %||% ex$source_id %||% fpa_txt("loc_extra_file"))
    sid <- as.character(ex$source_id %||% nm)
    if (!nzchar(p)) next
    if (file.exists(p)) next
    items[[length(items) + 1]] <- fpa_diag_item(
      role = sid,
      title = paste0(fpa_txt("loc_extra_file_open"), nm, ")"),
      file = basename(p),
      path = p,
      severity = "warning",
      code = "extra_missing",
      problem = fpa_txt("loc_path_missing_series"),
      where = "poste",
      step = "Chemins",
      action = c(
        paste0(fpa_txt("loc_put_file_here"), fpa_diag_show_path(p)),
        "Ou corrigez le chemin a l'etape Chemins, puis telechargez fpa-config.json."
      )
    )
  }
  items
}

#' Run the full pre-flight. Does not stop(); caller decides.
fpa_diagnose <- function(config) {
  items <- list()
  files <- list()
  for (role in names(FPA_DIAG_ROLES)) {
    p <- fpa_resolve_path(config, role)
    src <- fpa_source_for_role(config, role)
    title <- FPA_DIAG_ROLES[[role]]
    declared <- config$paths[[role]]
    if (is.null(declared) || !nzchar(as.character(declared))) next
    path <- if (is.na(p) || !nzchar(p)) as.character(declared) else p
    ok_file <- file.exists(path)
    files[[role]] <- list(
      role = role,
      title = title,
      file = basename(path),
      path = path,
      exists = ok_file
    )
    found <- fpa_diagnose_source(config, role, path, src, title)
    items <- c(items, found)
    if (!length(found) && ok_file) {
      items[[length(items) + 1]] <- fpa_diag_item(
        role, title, basename(path), path,
        severity = "ok", code = "ok",
        problem = fpa_txt("loc_file_mapping_ok"),
        where = "",
        step = "",
        action = ""
      )
    }
  }
  items <- c(items, fpa_diagnose_extras(config))
  sev <- vapply(items, function(it) it$severity, character(1))
  blocking <- any(sev == "error")
  list(
    items = items,
    files = files,
    blocking = blocking,
    n_error = sum(sev == "error"),
    n_warning = sum(sev == "warning"),
    n_ok = sum(sev == "ok")
  )
}

fpa_diagnose_print <- function(report, config_path = NULL, working = NULL) {
  cat("\n")
  cat(fpa_diag_rule("="))
  cat("  FPA local  |  Controle fichiers / reglages\n")
  cat(fpa_diag_rule("="))
  if (!is.null(config_path) && nzchar(config_path)) {
    cat("  Reglages : ", fpa_diag_show_path(config_path), "\n", sep = "")
  }
  if (!is.null(working) && nzchar(working)) {
    cat("  Dossier  : ", fpa_diag_show_path(working), "\n", sep = "")
  }
  cat("\n")
  cat("  Fichiers declares\n")
  cat("  ", strrep("-", 18), "\n", sep = "")

  # One status line per role: worst severity for that role
  roles_seen <- unique(vapply(report$items, function(it) it$role, character(1)))
  for (role in names(FPA_DIAG_ROLES)) {
    its <- Filter(function(it) identical(it$role, role), report$items)
    if (!length(its)) next
    sev <- vapply(its, function(it) it$severity, character(1))
    mark <- if (any(sev == "error")) "BLOQUANT" else if (any(sev == "warning")) "ATTENTION" else "OK"
    pad <- format(mark, width = 10, justify = "left")
    cat(sprintf("  %-10s  %-22s  %s\n", pad, its[[1]]$title, its[[1]]$file))
  }
  extra_its <- Filter(function(it) !(it$role %in% names(FPA_DIAG_ROLES)), report$items)
  for (it in extra_its) {
    mark <- if (identical(it$severity, "error")) "BLOQUANT" else if (identical(it$severity, "warning")) "ATTENTION" else "OK"
    cat(sprintf("  %-10s  %-22s  %s\n", format(mark, width = 10), it$title, it$file))
  }

  detail <- Filter(function(it) !identical(it$severity, "ok"), report$items)
  if (length(detail)) {
    cat("\n")
    cat(fpa_diag_rule("-"))
    cat("  Details\n")
    cat(fpa_diag_rule("-"))
    n <- length(detail)
    for (i in seq_along(detail)) {
      it <- detail[[i]]
      badge <- if (identical(it$severity, "error")) "BLOQUANT" else "ATTENTION"
      cat("\n")
      cat(sprintf("  [%d/%d]  %s\n", i, n, it$title))
      cat(sprintf(fpa_txt("loc_file_fmt"), it$file))
      if (nzchar(it$path)) {
        cat(sprintf("          chemin  : %s\n", fpa_diag_show_path(it$path)))
      }
      cat(sprintf("          statut  : %s\n", badge))
      cat("\n")
      cat("          Probleme\n")
      for (p in it$problem) {
        if (grepl("^            - ", p)) {
          cat(p, "\n", sep = "")
        } else {
          cat(paste(fpa_diag_wrap(p, indent = "            "), collapse = "\n"), "\n", sep = "")
        }
      }
      cat("\n")
      cat("          Ou corriger\n")
      cat(paste(fpa_diag_wrap(fpa_diag_where_line(it$where), indent = "            "), collapse = "\n"), "\n", sep = "")
      st <- as.character(it$step %||% "")
      if (nzchar(st) && st %in% names(FPA_DIAG_WIZARD)) {
        cat("            -> ", FPA_DIAG_WIZARD[[st]], "\n", sep = "")
      } else if (nzchar(st)) {
        cat("            -> ", st, "\n", sep = "")
      }
      cat("\n")
      cat("          Que faire\n")
      acts <- it$action
      acts <- acts[nzchar(acts)]
      for (a in acts) {
        wrapped <- fpa_diag_wrap(a, indent = "            ")
        if (length(wrapped)) {
          wrapped[[1]] <- sub("^            ", "            * ", wrapped[[1]])
          cat(paste(wrapped, collapse = "\n"), "\n", sep = "")
        }
      }
    }
  }

  cat("\n")
  cat(fpa_diag_rule("="))
  if (isTRUE(report$blocking)) {
    cat(sprintf(
      fpa_txt("loc_verdict_block"),
      report$n_error,
      if (report$n_warning) paste0(", ", report$n_warning, " attention") else ""
    ))
    cat(fpa_txt("loc_fix_in_order"))
  } else if (report$n_warning > 0) {
    cat(sprintf(
      fpa_txt("loc_verdict_warn"),
      report$n_warning
    ))
  } else {
    cat(fpa_txt("loc_verdict_ok"))
  }
  cat(fpa_diag_rule("="))
  if (any(vapply(report$items, function(it) identical(it$code, "missing_file"), logical(1))) &&
      !is.null(working) && nzchar(working)) {
    cat(
      "  Tables modeles : ",
      fpa_diag_show_path(file.path(working, "input", "modeles")),
      "\n\n",
      sep = ""
    )
  }
  cat("\n")
  invisible(report)
}

#' Write the same report to reports/diagnostic.txt (UTF-8).
fpa_diagnose_write <- function(report, paths, config_path = NULL) {
  dir.create(paths$reports, showWarnings = FALSE, recursive = TRUE)
  dest <- file.path(paths$reports, "diagnostic.txt")
  txt <- capture.output(fpa_diagnose_print(report, config_path, paths$working))
  writeLines(txt, dest, useBytes = FALSE)
  dest
}
