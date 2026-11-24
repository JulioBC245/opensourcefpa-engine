# SPDX-License-Identifier: AGPL-3.0-or-later
# Copyright (C) 2026 Julio Bauer Cantuaria
# fpa-freeze.R — Immutable budget, forecast vintages, summary from a stored vintage.
# LAST CHANGE: 2026-10-03 — close cockpit: version register, line gaps, lock merge and drift, budget check
# A rephase is a labelled view. It does not edit the frozen file.
# The summary is read back from the vintage. It does not recompute.

fpa_md5_file <- function(path) {
  if (!file.exists(path)) return(NA_character_)
  unname(tools::md5sum(path))
}

fpa_freeze_budget <- function(paths, source = file.path(paths$work, "budget-monthly.csv")) {
  if (!file.exists(source)) {
    stop(fpa_txt("loc_no_budget_freeze"), call. = FALSE)
  }
  dest_dir <- file.path(paths$work, "vintages")
  dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)
  frozen <- file.path(dest_dir, "budget-frozen.csv")
  file.copy(source, frozen, overwrite = TRUE)
  manifest <- list(
    kind = "budget",
    frozen_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S"),
    source = basename(source),
    md5 = fpa_md5_file(frozen),
    note = "Rephase is a separate labelled view. This file is not edited."
  )
  jsonlite::write_json(manifest, file.path(dest_dir, "budget-manifest.json"),
                       auto_unbox = TRUE, pretty = TRUE)
  invisible(manifest)
}

fpa_save_vintage <- function(paths, label = "forecast") {
  pnl_path <- file.path(paths$work, "pnl-monthly.csv")
  if (!file.exists(pnl_path)) stop(fpa_txt("loc_no_pnl_archive"), call. = FALSE)
  id <- format(Sys.time(), "%Y%m%d-%H%M%S")
  dest <- file.path(paths$work, "vintages", id)
  dir.create(dest, recursive = TRUE, showWarnings = FALSE)
  file.copy(pnl_path, file.path(dest, "pnl-monthly.csv"), overwrite = TRUE)
  cash_path <- file.path(paths$work, "cash-bridge-monthly.csv")
  if (file.exists(cash_path)) {
    file.copy(cash_path, file.path(dest, "cash-bridge-monthly.csv"), overwrite = TRUE)
  }
  manifest <- list(
    id = id,
    label = label,
    saved_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S"),
    pnl_md5 = fpa_md5_file(file.path(dest, "pnl-monthly.csv"))
  )
  jsonlite::write_json(manifest, file.path(dest, "manifest.json"),
                       auto_unbox = TRUE, pretty = TRUE)
  invisible(manifest)
}

fpa_compare_vintages <- function(paths, id_a, id_b) {
  read_one <- function(id) {
    p <- file.path(paths$work, "vintages", id, "pnl-monthly.csv")
    if (!file.exists(p)) stop(fpa_txt("loc_stop_missing"), id, call. = FALSE)
    utils::read.csv(p, stringsAsFactors = FALSE)
  }
  a <- read_one(id_a)
  b <- read_one(id_b)
  both <- intersect(a$periode, b$periode)
  if (!length(both) || !("OI" %in% names(a))) {
    return(data.frame())
  }
  data.frame(
    periode = both,
    oi_a = a$OI[match(both, a$periode)],
    oi_b = b$OI[match(both, b$periode)],
    delta_oi = b$OI[match(both, b$periode)] - a$OI[match(both, a$periode)],
    stringsAsFactors = FALSE
  )
}

#' Month with the largest OI delta between two vintages, same voice as Écarts.
fpa_compare_headline <- function(cmp, abs_floor = 1000) {
  if (is.null(cmp) || !nrow(cmp)) return(fpa_txt("close_headline_none"))
  d <- suppressWarnings(as.numeric(cmp$delta_oi))
  ok <- is.finite(d) & abs(d) > abs_floor
  if (!any(ok)) return(fpa_txt("close_headline_none"))
  i <- which(ok)[which.max(abs(d[ok]))]
  amt <- d[[i]]
  fav <- if (amt >= 0) fpa_txt("var_fav") else fpa_txt("var_unfav")
  fpa_txt("close_headline", fav, fpa_synthese_eur(amt), cmp$periode[[i]])
}

fpa_close_op_lines <- function() c("REV", "OTHER_OP_INC", "COGS", "PAYROLL", "OPEX_OTHER", "D_A")
fpa_close_below_lines <- function() c("FIN", "EXCEP", "TAX")

#' P&L of a stored version, or of the current calculation when id is "current".
fpa_close_read_pnl <- function(paths, id) {
  p <- if (identical(id, "current")) {
    file.path(paths$work, "pnl-monthly.csv")
  } else {
    file.path(paths$work, "vintages", id, "pnl-monthly.csv")
  }
  if (!file.exists(p)) return(NULL)
  utils::read.csv(p, stringsAsFactors = FALSE)
}

fpa_close_read_cash <- function(paths, id) {
  p <- if (identical(id, "current")) {
    file.path(paths$work, "cash-bridge-monthly.csv")
  } else {
    file.path(paths$work, "vintages", id, "cash-bridge-monthly.csv")
  }
  if (!file.exists(p)) return(NULL)
  utils::read.csv(p, stringsAsFactors = FALSE)
}

fpa_close_sum <- function(df, col, months) {
  if (is.null(df) || !nrow(df) || !(col %in% names(df))) return(NA_real_)
  v <- suppressWarnings(as.numeric(df[[col]][as.character(df$periode) %in% months]))
  if (!length(v)) return(NA_real_)
  sum(v, na.rm = TRUE)
}

fpa_close_cash_end <- function(cash, months) {
  if (is.null(cash) || !nrow(cash) || !("cash_close" %in% names(cash))) return(NA_real_)
  hit <- intersect(sort(as.character(months), decreasing = TRUE), as.character(cash$periode))
  if (!length(hit)) return(NA_real_)
  suppressWarnings(as.numeric(cash$cash_close[match(hit[[1]], as.character(cash$periode))]))
}

#' Every saved version, oldest first, with its totals over `months` (the current plan year).
#' `same_as_prev` marks a byte-for-byte copy of the version saved just before it.
fpa_close_versions <- function(paths, months) {
  root <- file.path(paths$work, "vintages")
  if (!dir.exists(root)) return(data.frame())
  rows <- list()
  for (id in list.dirs(root, full.names = FALSE, recursive = FALSE)) {
    man <- file.path(root, id, "manifest.json")
    if (!file.exists(man)) next
    m <- tryCatch(jsonlite::read_json(man), error = function(e) NULL)
    if (is.null(m)) next
    pnl <- fpa_close_read_pnl(paths, id)
    md5 <- as.character(m$pnl_md5 %||% "")
    if (!nzchar(md5)) md5 <- fpa_md5_file(file.path(root, id, "pnl-monthly.csv"))
    rows[[length(rows) + 1L]] <- data.frame(
      id = id,
      label = as.character(m$label %||% ""),
      saved_at = as.character(m$saved_at %||% ""),
      md5 = as.character(md5),
      n_months = if (is.null(pnl)) 0L else length(intersect(as.character(pnl$periode), months)),
      rev = fpa_close_sum(pnl, "REV", months),
      oi = fpa_close_sum(pnl, "OI", months),
      ni = fpa_close_sum(pnl, "NI", months),
      cash_end = fpa_close_cash_end(fpa_close_read_cash(paths, id), months),
      stringsAsFactors = FALSE
    )
  }
  if (!length(rows)) return(data.frame())
  out <- do.call(rbind, rows)
  out <- out[order(out$saved_at, out$id), , drop = FALSE]
  rownames(out) <- NULL
  out$same_as_prev <- c(FALSE, out$md5[-1L] == out$md5[-nrow(out)])
  out
}

#' Default pair: the latest version against the last earlier one whose file differs.
#' When the current calculation is not saved yet, it is the arrival.
fpa_close_default_pair <- function(versions, current_md5) {
  if (is.null(versions) || !nrow(versions)) return(c(a = "", b = ""))
  saved <- current_md5 %in% versions$md5
  b <- if (saved) versions$id[[max(which(versions$md5 == current_md5))]] else "current"
  b_md5 <- if (identical(b, "current")) current_md5 else versions$md5[versions$id == b][[1]]
  cand <- which(versions$md5 != b_md5)
  if (identical(b, "current")) {
    cand <- seq_len(nrow(versions))
  } else {
    cand <- cand[cand < match(b, versions$id)]
  }
  a <- if (length(cand)) versions$id[[max(cand)]] else versions$id[[1]]
  c(a = a, b = b)
}

#' Line-by-month gap between two P&Ls, as the effect on the result.
#' A charge that rises counts negative, so the operating lines sum to the OI gap.
fpa_close_compare <- function(a, b, months = NULL) {
  if (is.null(a) || is.null(b) || !nrow(a) || !nrow(b)) return(NULL)
  both <- intersect(as.character(a$periode), as.character(b$periode))
  if (!is.null(months)) both <- intersect(both, as.character(months))
  both <- sort(both)
  if (!length(both)) return(NULL)
  pick <- function(df, col) {
    if (!(col %in% names(df))) return(rep(0, length(both)))
    v <- suppressWarnings(as.numeric(df[[col]][match(both, as.character(df$periode))]))
    v[!is.finite(v)] <- 0
    v
  }
  lines <- c(fpa_close_op_lines(), fpa_close_below_lines())
  lines <- lines[lines %in% names(a) | lines %in% names(b)]
  impact <- t(vapply(lines, function(l) (pick(b, l) - pick(a, l)) * fpa_pnl_ni_sign(l), numeric(length(both))))
  if (length(both) == 1L) impact <- matrix(impact, ncol = 1L, dimnames = list(lines, NULL))
  colnames(impact) <- both
  list(
    months = both,
    lines = lines,
    impact = impact,
    oi = pick(b, "OI") - pick(a, "OI"),
    ni = pick(b, "NI") - pick(a, "NI"),
    oi_a = sum(pick(a, "OI")), oi_b = sum(pick(b, "OI")),
    ni_a = sum(pick(a, "NI")), ni_b = sum(pick(b, "NI")),
    rev_a = sum(pick(a, "REV")), rev_b = sum(pick(b, "REV")),
    moved = both[colSums(abs(impact) >= 0.5) > 0]
  )
}

#' Closed months not in the lock yet, and lock rows merged without touching locked amounts.
fpa_close_lock_merge <- function(old, new) {
  if (is.null(old) || !nrow(old)) return(new)
  key_old <- paste(old$periode, old$line_id)
  add <- new[!(paste(new$periode, new$line_id) %in% key_old), , drop = FALSE]
  out <- rbind(old[, c("periode", "line_id", "montant")], add[, c("periode", "line_id", "montant")])
  out[order(out$periode, match(out$line_id, fpa_pnl_input_ids())), , drop = FALSE]
}

#' P&L built from the source files only, before the lock is applied.
fpa_close_source_pnl <- function(config, paths) {
  config <- tryCatch(fpa_apply_local_overlay(config, paths), error = function(e) config)
  reads <- fpa_read_all_financial(config, paths)
  mapped <- NULL
  if (!is.null(reads$pnl_monthly) && isTRUE(reads$pnl_monthly$ok) && nrow(reads$pnl_monthly$mapped %||% data.frame())) {
    mapped <- reads$pnl_monthly$mapped
  } else if (!is.null(reads$pnl_actuals) && isTRUE(reads$pnl_actuals$ok)) {
    mapped <- reads$pnl_actuals$mapped
  }
  src <- NULL
  if (!is.null(mapped) && nrow(mapped)) {
    mapped <- fpa_drop_empty_tail(mapped)$mapped
    periods <- sort(unique(format(as.Date(mapped$period_end), "%Y-%m")))
    src <- fpa_build_pnl(mapped, periods = periods)
  }
  hist <- tryCatch(fpa_history_pnl(config, paths), error = function(e) NULL)
  if (!is.null(hist) && nrow(hist)) {
    extra <- hist[!(as.character(hist$periode) %in% as.character(src$periode %||% character(0))), , drop = FALSE]
    if (is.null(src)) {
      src <- extra
    } else if (nrow(extra)) {
      cols <- intersect(names(src), names(extra))
      src <- rbind(src[, cols, drop = FALSE], extra[, cols, drop = FALSE])
    }
  }
  if (is.null(src)) return(NULL)
  src[order(as.character(src$periode)), , drop = FALSE]
}

#' Locked amounts the source file now states differently. One row per month and line.
fpa_close_drift <- function(lock, src, tol = 0.5) {
  empty <- data.frame(periode = character(0), line_id = character(0), locked = numeric(0), source = numeric(0), delta = numeric(0))
  if (is.null(lock) || !nrow(lock) || is.null(src) || !nrow(src)) return(empty)
  rows <- lapply(seq_len(nrow(lock)), function(i) {
    p <- as.character(lock$periode[[i]])
    l <- as.character(lock$line_id[[i]])
    if (!(l %in% names(src))) return(NULL)
    j <- match(p, as.character(src$periode))
    if (is.na(j)) return(NULL)
    s <- suppressWarnings(as.numeric(src[[l]][[j]]))
    k <- suppressWarnings(as.numeric(lock$montant[[i]]))
    if (!is.finite(s) || !is.finite(k) || abs(s - k) < tol) return(NULL)
    data.frame(periode = p, line_id = l, locked = k, source = s, delta = s - k, stringsAsFactors = FALSE)
  })
  rows <- Filter(Negate(is.null), rows)
  if (!length(rows)) return(empty)
  do.call(rbind, rows)
}

#' Take the source amounts for the drifted rows. Other locked amounts stay as they are.
fpa_close_accept_drift <- function(paths, drift) {
  lock_path <- file.path(paths$work, "standing-lock.csv")
  if (!file.exists(lock_path) || is.null(drift) || !nrow(drift)) return(0L)
  lock <- utils::read.csv(lock_path, stringsAsFactors = FALSE)
  key <- paste(lock$periode, lock$line_id)
  hit <- match(paste(drift$periode, drift$line_id), key)
  ok <- !is.na(hit)
  lock$montant[hit[ok]] <- drift$source[ok]
  utils::write.csv(lock, lock_path, row.names = FALSE)
  sum(ok)
}

#' Frozen budget against the budget of the current calculation, by line over the year.
fpa_close_budget_check <- function(paths) {
  root <- file.path(paths$work, "vintages")
  man_path <- file.path(root, "budget-manifest.json")
  frozen_path <- file.path(root, "budget-frozen.csv")
  cur_path <- file.path(paths$work, "budget-monthly.csv")
  man <- if (file.exists(man_path)) tryCatch(jsonlite::read_json(man_path), error = function(e) NULL) else NULL
  frozen_ok <- !is.null(man) && file.exists(frozen_path)
  cur_md5 <- fpa_md5_file(cur_path)
  totals <- function(path) {
    if (!file.exists(path)) return(NULL)
    df <- utils::read.csv(path, stringsAsFactors = FALSE)
    if (!all(c("functional_label", "montant") %in% names(df))) return(NULL)
    agg <- tapply(suppressWarnings(as.numeric(df$montant)), as.character(df$functional_label), sum, na.rm = TRUE)
    stats::setNames(as.numeric(agg), names(agg))
  }
  f <- if (frozen_ok) totals(frozen_path) else NULL
  c_ <- totals(cur_path)
  ids <- intersect(fpa_pnl_input_ids(), union(names(f), names(c_)))
  tab <- data.frame(
    line_id = ids,
    frozen = if (is.null(f)) rep(NA_real_, length(ids)) else unname(f[ids]),
    current = if (is.null(c_)) rep(NA_real_, length(ids)) else unname(c_[ids]),
    stringsAsFactors = FALSE
  )
  tab$delta <- tab$current - tab$frozen
  list(
    frozen = frozen_ok,
    frozen_at = if (frozen_ok) as.character(man$frozen_at %||% "") else "",
    intact = frozen_ok && !is.na(cur_md5) && identical(tolower(as.character(man$md5 %||% "")), tolower(cur_md5)),
    has_current = file.exists(cur_path),
    table = tab
  )
}

#' Management summary from a stored vintage. Reads the file. Does not rerun the engine.
fpa_export_summary <- function(paths, vintage_id) {
  src <- file.path(paths$work, "vintages", vintage_id, "pnl-monthly.csv")
  if (!file.exists(src)) stop(fpa_txt("loc_stop_missing"), vintage_id, call. = FALSE)
  pnl <- utils::read.csv(src, stringsAsFactors = FALSE)
  summary <- data.frame(
    vintage_id = vintage_id,
    months = nrow(pnl),
    rev = sum(pnl$REV, na.rm = TRUE),
    oi = sum(pnl$OI, na.rm = TRUE),
    ni = sum(pnl$NI, na.rm = TRUE),
    stringsAsFactors = FALSE
  )
  dest <- file.path(paths$reports, paste0("summary-", vintage_id, ".csv"))
  dir.create(paths$reports, recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(summary, dest, row.names = FALSE)
  invisible(summary)
}
