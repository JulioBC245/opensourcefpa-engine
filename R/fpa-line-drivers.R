# SPDX-License-Identifier: AGPL-3.0-or-later
# Copyright (C) 2026 Julio Bauer Cantuaria
# fpa-line-drivers.R — What each charge line follows, measured on the monthly history.
# LAST CHANGE: 2026-09-23
# The setup page writes line_drivers (one driver per statement line, no amounts).
# The engine measures the coefficients on the longest monthly history in input/:
#   volume       per unit of volume (nights, units, covers)
#   mixed        fixed per month + per unit of volume (least squares)
#   revenue_pct  share of revenue
#   contract     flat monthly amount
# Every other driver, and every line left open, stays on the seasonal path.

FPA_DRIVEN <- c("volume", "mixed", "revenue_pct", "contract")

#' Driver rows from the configuration. Empty data frame when none.
fpa_line_driver_rows <- function(config) {
  rows <- config$line_drivers$lines %||% list()
  if (is.data.frame(rows)) rows <- split(rows, seq_len(nrow(rows)))
  out <- lapply(rows, function(r) {
    data.frame(
      user_label = as.character(r$user_label %||% ""),
      functional_label = as.character(r$functional_label %||% ""),
      driver = as.character(r$driver %||% "unknown"),
      origin = as.character(r$origin %||% ""),
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, out)
  if (is.null(out) || !nrow(out)) {
    return(data.frame(user_label = character(), functional_label = character(),
                      driver = character(), origin = character(), stringsAsFactors = FALSE))
  }
  out[nzchar(out$user_label), , drop = FALSE]
}

#' Long history of statement lines, before they are summed into buckets.
fpa_line_history <- function(config, paths = fpa_paths(config)) {
  dir <- paths$input
  if (!dir.exists(dir)) return(NULL)
  files <- list.files(dir, pattern = "pnl.*\\.csv$", full.names = TRUE, ignore.case = TRUE)
  best <- NULL
  best_n <- 0L
  for (path in files) {
    df <- tryCatch(
      utils::read.csv(path, stringsAsFactors = FALSE, fileEncoding = "UTF-8", check.names = FALSE),
      error = function(e) NULL
    )
    if (is.null(df) || ncol(df) < 3L) next
    periode <- as.character(df[[2]])
    ok <- grepl("^[0-9]{4}-[0-9]{2}$", periode)
    n <- length(unique(periode[ok]))
    if (n <= best_n) next
    best <- data.frame(
      libelle = as.character(df[[1]])[ok],
      periode = periode[ok],
      montant = suppressWarnings(as.numeric(df[[3]]))[ok],
      stringsAsFactors = FALSE
    )
    best <- best[is.finite(best$montant), , drop = FALSE]
    best_n <- n
  }
  best
}

#' Activity volume by month: nights sold, else capacity x utilisation, else units, else customers.
fpa_volume_by_month <- function(vol_df) {
  if (is.null(vol_df) || !nrow(vol_df) || !("periode" %in% names(vol_df))) return(setNames(numeric(0), character(0)))
  num <- function(col) suppressWarnings(as.numeric(vol_df[[col]]))
  v <- rep(NA_real_, nrow(vol_df))
  if ("nuitees_vendues" %in% names(vol_df)) v <- num("nuitees_vendues")
  if (all(c("capacite", "taux_utilisation") %in% names(vol_df))) {
    alt <- num("capacite") * num("taux_utilisation")
    v[!is.finite(v)] <- alt[!is.finite(v)]
  }
  if ("quantite" %in% names(vol_df)) v[!is.finite(v)] <- num("quantite")[!is.finite(v)]
  if ("nb_clients" %in% names(vol_df)) v[!is.finite(v)] <- num("nb_clients")[!is.finite(v)]
  agg <- tapply(v, as.character(vol_df$periode), function(x) sum(x, na.rm = TRUE))
  agg <- agg[is.finite(agg)]
  setNames(as.numeric(agg), names(agg))
}

#' Coefficients for every driven line. One row per configured line.
fpa_line_driver_fit <- function(rows, history, volume, rev_by_month, before = NULL, window = 36L) {
  empty <- data.frame(
    user_label = character(), functional_label = character(), driver = character(),
    fixed = numeric(), per_volume = numeric(), pct_rev = numeric(),
    n = integer(), r2 = numeric(), method = character(), stringsAsFactors = FALSE
  )
  if (is.null(rows) || !nrow(rows) || is.null(history) || !nrow(history)) return(empty)
  hist_fold <- fpa_fold_label(history$libelle)
  out <- vector("list", nrow(rows))
  for (i in seq_len(nrow(rows))) {
    r <- rows[i, , drop = FALSE]
    fit <- list(fixed = 0, per_volume = 0, pct_rev = 0, n = 0L, r2 = NA_real_, method = "seasonal")
    if (r$driver %in% FPA_DRIVEN) {
      sl <- history[hist_fold == fpa_fold_label(r$user_label), , drop = FALSE]
      if (length(before) && nzchar(before)) sl <- sl[sl$periode < before, , drop = FALSE]
      agg <- tapply(sl$montant, sl$periode, sum)
      pers <- sort(names(agg))
      pers <- utils::tail(pers, window)
      y <- as.numeric(agg[pers])
      x <- as.numeric(volume[pers])
      rv <- as.numeric(rev_by_month[pers])
      if (length(y) >= 3L) {
        fit$n <- length(y)
        if (r$driver == "contract") {
          fit$fixed <- stats::median(utils::tail(y, 3L))
          fit$method <- "flat_last_3"
        } else if (r$driver == "revenue_pct") {
          ok <- is.finite(rv) & abs(rv) > 1
          last <- utils::tail(which(ok), 12L)
          if (length(last) >= 3L) {
            fit$pct_rev <- sum(y[last]) / sum(rv[last])
            fit$method <- "ratio_last_12"
          }
        } else {
          ok <- is.finite(x) & x > 0 & is.finite(y)
          if (sum(ok) >= 3L) {
            last <- utils::tail(which(ok), 12L)
            ratio <- sum(y[last]) / sum(x[last])
            if (r$driver == "volume") {
              fit$per_volume <- ratio
              fit$method <- "ratio_last_12"
            } else if (sum(ok) >= 12L) {
              m <- stats::lm(y[ok] ~ x[ok])
              a <- unname(stats::coef(m)[[1]])
              b <- unname(stats::coef(m)[[2]])
              fit$r2 <- suppressWarnings(summary(m)$r.squared)
              if (!is.finite(b) || b <= 0) {
                fit$fixed <- mean(utils::tail(y[ok], 12L))
                fit$method <- "fit_flat"
              } else if (!is.finite(a) || a < 0) {
                fit$per_volume <- ratio
                fit$method <- "fit_ratio"
              } else {
                fit$fixed <- a
                fit$per_volume <- b
                fit$method <- "fit_ols"
              }
            } else {
              fit$per_volume <- ratio
              fit$method <- "ratio_short_history"
            }
          }
        }
      }
      if (fit$method == "seasonal") fit$n <- 0L
    }
    out[[i]] <- data.frame(
      user_label = r$user_label, functional_label = r$functional_label, driver = r$driver,
      fixed = fit$fixed, per_volume = fit$per_volume, pct_rev = fit$pct_rev,
      n = as.integer(fit$n), r2 = fit$r2, method = fit$method, stringsAsFactors = FALSE
    )
  }
  res <- do.call(rbind, out)
  if (is.null(res)) empty else res
}

#' One driven line in one month.
fpa_line_driver_value <- function(fit_row, volume, revenue, factor = 1) {
  v <- if (is.finite(volume)) volume else 0
  rv <- if (is.finite(revenue)) revenue else 0
  fit_row$fixed + factor * fit_row$per_volume * v + factor * fit_row$pct_rev * rv
}

#' Share of a bucket carried by the lines that stay seasonal, same calendar month in history.
fpa_line_rest_share <- function(history, fit, bucket, month) {
  lines <- fit[fit$functional_label == bucket, , drop = FALSE]
  if (!nrow(lines)) return(1)
  rest <- lines$user_label[lines$method == "seasonal"]
  hist_fold <- fpa_fold_label(history$libelle)
  in_bucket <- hist_fold %in% fpa_fold_label(lines$user_label)
  sl <- history[in_bucket & substr(history$periode, 6, 7) == sprintf("%02d", month), , drop = FALSE]
  if (!nrow(sl)) return(if (length(rest)) 1 else 0)
  total <- sum(sl$montant)
  if (!is.finite(total) || abs(total) < 1) return(if (length(rest)) 1 else 0)
  part <- sum(sl$montant[fpa_fold_label(sl$libelle) %in% fpa_fold_label(rest)])
  max(0, min(1, part / total))
}

#' Replace each driven bucket on the open months.
#' Bucket = seasonal figure x share of the lines left seasonal + sum of driven lines.
fpa_pnl_apply_line_drivers <- function(pnl, fit, history, volume, forecast_periods) {
  if (is.null(fit) || !nrow(fit) || !length(forecast_periods)) return(pnl)
  driven <- fit[fit$method != "seasonal", , drop = FALSE]
  if (!nrow(driven)) return(pnl)
  for (bucket in intersect(unique(driven$functional_label), names(pnl))) {
    lines <- driven[driven$functional_label == bucket, , drop = FALSE]
    for (p in as.character(forecast_periods)) {
      i <- match(p, pnl$periode)
      if (is.na(i)) next
      share <- fpa_line_rest_share(history, fit, bucket, as.integer(substr(p, 6, 7)))
      seasonal <- suppressWarnings(as.numeric(pnl[[bucket]][[i]]))
      if (!is.finite(seasonal)) seasonal <- 0
      add <- 0
      for (k in seq_len(nrow(lines))) {
        add <- add + fpa_line_driver_value(lines[k, , drop = FALSE], volume[p], pnl$REV[[i]])
      }
      pnl[[bucket]][[i]] <- seasonal * share + add
    }
  }
  need <- c("REV", "OTHER_OP_INC", "COGS", "PAYROLL", "OPEX_OTHER", "D_A", "FIN", "EXCEP", "TAX")
  if (all(need %in% names(pnl))) pnl <- fpa_pnl_derive(pnl)
  pnl
}
