# SPDX-License-Identifier: AGPL-3.0-or-later
# Copyright (C) 2026 Julio Bauer Cantuaria
# fpa-anomaly.R — What a close review would stop on.
# LAST CHANGE: 2026-09-23
# Not a fraud score. Four questions: do the books tie, does the statement
# meet the operating driver, is this month unlike the same month in prior
# years, is a ratio large only because the denominator is almost nothing.

FPA_ANOMALY_FLOOR <- 1000

fpa_anomaly_empty <- function() {
  data.frame(
    family = character(), level = character(), periode = character(),
    line_id = character(), value = numeric(), reference = numeric(), gap = numeric(),
    n = integer(),
    stringsAsFactors = FALSE
  )
}

fpa_anomaly_row <- function(family, level, periode, line_id, value, reference, gap, n = 1L) {
  data.frame(
    family = family, level = level, periode = as.character(periode),
    line_id = as.character(line_id),
    value = as.numeric(value), reference = as.numeric(reference), gap = as.numeric(gap),
    n = as.integer(n),
    stringsAsFactors = FALSE
  )
}

#' Many months of one line are one finding: the widest gap, and how many months.
fpa_anomaly_collapse <- function(df, min_n = 4L) {
  if (is.null(df) || !nrow(df)) return(df)
  if (!"n" %in% names(df)) df$n <- 1L
  keep <- list()
  for (line in unique(df$line_id)) {
    sl <- df[df$line_id == line, , drop = FALSE]
    if (nrow(sl) < min_n) {
      keep[[length(keep) + 1L]] <- sl
    } else {
      top <- sl[which.max(abs(sl$gap)), , drop = FALSE]
      top$n <- nrow(sl)
      keep[[length(keep) + 1L]] <- top
    }
  }
  out <- do.call(rbind, keep)
  rownames(out) <- NULL
  out
}

fpa_anomaly_assets <- function(bs, i) {
  z <- function(col) {
    x <- if (col %in% names(bs)) suppressWarnings(as.numeric(bs[[col]][[i]])) else 0
    if (is.finite(x)) x else 0
  }
  list(
    assets = z("CASH") + z("AR") + z("INV") + z("PPE") + z("OTHER_CA"),
    claims = z("AP") + z("ST_DEBT") + z("LT_DEBT") + z("OTHER_CL") + z("EQUITY"),
    cash = z("CASH"),
    cl = z("AP") + z("ST_DEBT") + z("OTHER_CL")
  )
}

fpa_anomaly_books <- function(bs, cash, tol = 1) {
  rows <- list()
  if (!is.null(bs) && nrow(bs)) {
    for (i in seq_len(nrow(bs))) {
      parts <- fpa_anomaly_assets(bs, i)
      gap <- parts$assets - parts$claims
      if (is.finite(gap) && abs(gap) > tol) {
        rows[[length(rows) + 1L]] <- fpa_anomaly_row(
          "books", "break", bs$periode[[i]], "BS", parts$assets, parts$claims, gap
        )
      }
    }
  }
  if (!is.null(cash) && nrow(cash) && all(c("cash_open", "delta_cash", "cash_close", "periode") %in% names(cash))) {
    for (i in seq_len(nrow(cash))) {
      open <- suppressWarnings(as.numeric(cash$cash_open[[i]]))
      delta <- suppressWarnings(as.numeric(cash$delta_cash[[i]]))
      close <- suppressWarnings(as.numeric(cash$cash_close[[i]]))
      if (!is.finite(open) || !is.finite(delta) || !is.finite(close)) next
      gap <- close - (open + delta)
      if (abs(gap) > tol) {
        rows[[length(rows) + 1L]] <- fpa_anomaly_row(
          "books", "break", cash$periode[[i]], "CASH", close, open + delta, gap
        )
      }
    }
  }
  if (!length(rows)) {
    return(fpa_anomaly_row("books", "clear", "", "BS", NA_real_, NA_real_, 0))
  }
  do.call(rbind, rows)
}

fpa_anomaly_driver_expected <- function(volumes) {
  if (is.null(volumes) || !nrow(volumes) || !"periode" %in% names(volumes)) return(NULL)
  num <- function(col) {
    if (!col %in% names(volumes)) return(rep(NA_real_, nrow(volumes)))
    suppressWarnings(as.numeric(volumes[[col]]))
  }
  occ <- num("taux_utilisation")
  occ[is.finite(occ) & occ > 1.5] <- occ[is.finite(occ) & occ > 1.5] / 100
  data.frame(
    periode = as.character(volumes$periode),
    rev = num("capacite") * occ * num("tarif"),
    pay = num("effectif") * num("cout_moyen"),
    stringsAsFactors = FALSE
  )
}

fpa_anomaly_driver <- function(pnl, volumes, floor_eur = FPA_ANOMALY_FLOOR) {
  exp <- fpa_anomaly_driver_expected(volumes)
  if (is.null(exp) || is.null(pnl) || !nrow(pnl)) return(fpa_anomaly_empty())
  rows <- list()
  add <- function(line, per, value, reference, gap) {
    rows[[length(rows) + 1L]] <<- fpa_anomaly_row("driver", "watch", per, line, value, reference, gap)
  }
  check <- function(line, col) {
    if (!line %in% names(pnl)) return()
    for (i in seq_len(nrow(exp))) {
      per <- exp$periode[[i]]
      j <- match(per, as.character(pnl$periode))
      if (is.na(j)) next
      reference <- exp[[col]][[i]]
      value <- suppressWarnings(as.numeric(pnl[[line]][[j]]))
      if (!is.finite(reference) || !is.finite(value) || abs(reference) < 1) next
      gap <- value - reference
      if (abs(gap) >= floor_eur) add(line, per, value, reference, gap)
    }
  }
  check("REV", "rev")
  check("PAYROLL", "pay")
  if (!length(rows)) return(fpa_anomaly_empty())
  out <- do.call(rbind, rows)
  out[order(-abs(out$gap)), , drop = FALSE]
}

fpa_anomaly_bucket_month <- function(history, line_map, bucket) {
  if (is.null(history) || !nrow(history) || is.null(line_map) || !length(line_map)) return(NULL)
  keys <- names(line_map)[as.character(unlist(line_map)) == bucket]
  if (!length(keys)) return(NULL)
  hit <- fpa_fold_label(history$libelle) %in% fpa_fold_label(keys)
  sl <- history[hit, , drop = FALSE]
  if (!nrow(sl)) return(NULL)
  agg <- tapply(sl$montant, as.character(sl$periode), sum)
  data.frame(periode = names(agg), montant = as.numeric(agg), stringsAsFactors = FALSE)
}

fpa_anomaly_season <- function(pnl, history, line_map, floor_eur = FPA_ANOMALY_FLOOR) {
  if (is.null(pnl) || !nrow(pnl)) return(fpa_anomaly_empty())
  lines <- intersect(c("REV", "COGS", "PAYROLL", "OPEX_OTHER"), names(pnl))
  rows <- list()
  for (line in lines) {
    hist <- fpa_anomaly_bucket_month(history, line_map, line)
    if (is.null(hist) || !nrow(hist)) next
    for (i in seq_len(nrow(pnl))) {
      per <- as.character(pnl$periode[[i]])
      value <- suppressWarnings(as.numeric(pnl[[line]][[i]]))
      if (!is.finite(value)) next
      month <- substr(per, 6, 7)
      year <- substr(per, 1, 4)
      prior <- hist[substr(hist$periode, 6, 7) == month & substr(hist$periode, 1, 4) != year, , drop = FALSE]
      if (!nrow(prior)) next
      reference <- stats::median(prior$montant)
      gap <- value - reference
      if (!is.finite(reference) || abs(gap) < floor_eur) next
      if (abs(gap) < 0.25 * max(abs(reference), 1)) next
      rows[[length(rows) + 1L]] <- fpa_anomaly_row("season", "watch", per, line, value, reference, gap)
    }
  }
  if (!length(rows)) return(fpa_anomaly_empty())
  out <- fpa_anomaly_collapse(do.call(rbind, rows))
  out[order(-abs(out$gap)), , drop = FALSE]
}

#' One flag when cash dwarfs the bills. Not one flag per liquidity ratio.
fpa_anomaly_thin <- function(bs, focus = NULL) {
  if (is.null(bs) || !nrow(bs)) return(fpa_anomaly_empty())
  pers <- as.character(bs$periode)
  if (!is.null(focus) && length(focus)) pers <- intersect(pers, as.character(focus))
  rows <- list()
  for (per in pers) {
    i <- match(per, as.character(bs$periode))
    parts <- fpa_anomaly_assets(bs, i)
    if (parts$cl >= 1000 || parts$cash < 10000) next
    if (parts$cash < 10 * parts$cl) next
    ratio <- parts$cash / parts$cl
    rows[[length(rows) + 1L]] <- fpa_anomaly_row(
      "ratio", "watch", per, "cash_ratio", ratio, parts$cl, parts$cash
    )
  }
  if (!length(rows)) return(fpa_anomaly_empty())
  do.call(rbind, rows)
}

fpa_anomaly_scan <- function(bs, pnl, cash = NULL, volumes = NULL, history = NULL, line_map = NULL, focus = NULL, floor_eur = FPA_ANOMALY_FLOOR) {
  parts <- list(
    fpa_anomaly_books(bs, cash),
    fpa_anomaly_driver(pnl, volumes, floor_eur),
    fpa_anomaly_season(pnl, history, line_map, floor_eur),
    fpa_anomaly_thin(bs, focus)
  )
  parts <- parts[vapply(parts, function(x) !is.null(x) && nrow(x) > 0, logical(1))]
  if (!length(parts)) return(fpa_anomaly_empty())
  out <- do.call(rbind, parts)
  rownames(out) <- NULL
  out
}
