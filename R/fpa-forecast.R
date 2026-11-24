# SPDX-License-Identifier: AGPL-3.0-or-later
# Copyright (C) 2026 Julio Bauer Cantuaria
# fpa-forecast.R — Extend the engine past the last actual.
# LAST CHANGE: 2026-09-21
# Absent horizon_months means 0: a historical run stays historical.
# "fy" fills the rest of the fiscal year. A positive integer is a month count.
# A forecast line with neither a mechanism nor an assumption is refused.
# A zero carried forward is not a forecast.

fpa_horizon_months <- function(config, periods, override = NULL) {
  spec <- if (!is.null(override)) override else config$horizon_months
  if (is.null(spec) || (length(spec) == 1L && is.na(spec))) return(0L)
  if (is.numeric(spec)) return(max(0L, as.integer(spec)))
  if (!identical(as.character(spec), "fy") || !length(periods)) return(0L)
  fy_end <- as.integer(config$fiscal_year_end_month %||% config$fy_end_month %||% 12L)
  if (is.na(fy_end) || fy_end < 1L || fy_end > 12L) fy_end <- 12L
  last <- as.Date(paste0(max(periods), "-01"))
  month <- as.integer(format(last, "%m"))
  year <- as.integer(format(last, "%Y"))
  end_year <- if (month <= fy_end) year else year + 1L
  end <- as.Date(paste0(end_year, sprintf("-%02d-01", fy_end)))
  n <- length(seq(last, end, by = "month")) - 1L
  max(0L, as.integer(n))
}

#' Trailing months whose imported amounts are all ~0 are empty columns, not actuals.
fpa_drop_empty_tail <- function(mapped, tol = 0.5) {
  empty <- list(mapped = mapped, dropped = character())
  if (is.null(mapped) || !nrow(mapped) || !all(c("period_end", "montant") %in% names(mapped))) {
    return(empty)
  }
  pers <- format(as.Date(mapped$period_end), "%Y-%m")
  uniq <- sort(unique(pers))
  alive <- vapply(uniq, function(p) {
    v <- suppressWarnings(as.numeric(mapped$montant[pers == p]))
    any(is.finite(v) & abs(v) >= tol)
  }, logical(1))
  if (!any(alive)) return(empty)
  last <- max(which(alive))
  if (last >= length(uniq)) return(empty)
  drop <- uniq[(last + 1L):length(uniq)]
  list(mapped = mapped[!pers %in% drop, , drop = FALSE], dropped = drop)
}

fpa_extend_periods <- function(periods, n) {
  periods <- as.character(periods)
  n <- as.integer(n %||% 0L)
  if (!length(periods) || is.na(n) || n < 1L) return(periods)
  last <- as.Date(paste0(max(periods), "-01"))
  extra <- format(seq(last, by = "month", length.out = n + 1L)[-1], "%Y-%m")
  c(periods, extra)
}

#' Growth on revenue, only on forecast months, only when the mechanism is on.
fpa_pnl_apply_growth <- function(pnl, assum_wide, config, forecast_periods) {
  if (!length(forecast_periods)) return(pnl)
  if (!fpa_mechanism_on(config, "revenue_growth_pct")) return(pnl)
  if (!("REV" %in% names(pnl))) return(pnl)
  for (p in forecast_periods) {
    i <- which(pnl$periode == p)
    if (!length(i) || i[[1]] < 2L) next
    prev <- pnl$REV[[i[[1]] - 1L]]
    g <- fpa_assumption_num(assum_wide, p, "croissance.rev", default = 0)
    pnl$REV[[i[[1]]]] <- prev * (1 + g)
  }
  fpa_pnl_derive(pnl)
}

#' An assumption id prevision.<LINE> is an explicit management assumption.
fpa_pnl_apply_forecast_assumptions <- function(pnl, assum_wide, forecast_periods) {
  if (!length(forecast_periods) || is.null(assum_wide) || !nrow(assum_wide)) return(pnl)
  ids <- fpa_pnl_input_ids()
  for (id in ids) {
    key <- paste0("prevision.", id)
    if (!(key %in% names(assum_wide))) next
    for (p in forecast_periods) {
      i <- which(pnl$periode == p)
      if (!length(i)) next
      pnl[[id]][[i[[1]]]] <- fpa_assumption_num(assum_wide, p, key, default = pnl[[id]][[i[[1]]]])
    }
  }
  fpa_pnl_derive(pnl)
}

#' Material historical lines that are still zero on a forecast month.
#' explained: lines a mechanism already owns, including a real zero (a loss month has no tax).
fpa_forecast_refusals <- function(pnl, actual_periods, imported_ids, explained = character(0)) {
  forecast <- setdiff(pnl$periode, actual_periods)
  if (!length(forecast)) return(character(0))
  msgs <- character(0)
  ids <- setdiff(intersect(as.character(imported_ids), fpa_pnl_input_ids()), as.character(explained))
  for (id in ids) {
    hist <- pnl[[id]][pnl$periode %in% actual_periods]
    if (!any(abs(hist) > 1, na.rm = TRUE)) next
    for (p in forecast) {
      v <- pnl[[id]][pnl$periode == p]
      if (!length(v) || !is.finite(v[[1]]) || abs(v[[1]]) < 1e-6) {
        msgs <- c(msgs, paste0(
          id, " en ", p,
          fpa_txt("loc_no_mech_zero")
        ))
      }
    }
  }
  msgs
}

fpa_period_status <- function(periods, actual_periods) {
  data.frame(
    periode = as.character(periods),
    status = ifelse(as.character(periods) %in% as.character(actual_periods), "actual", "forecast"),
    stringsAsFactors = FALSE
  )
}

#' Same calendar month last year, only on a forecast month that is still empty.
#' Used when the line's phasing method is a seasonal index.
fpa_pnl_apply_seasonal_forecast <- function(pnl, forecast_periods, line_ids) {
  if (!length(forecast_periods) || !length(line_ids)) return(pnl)
  forecast_periods <- sort(unique(as.character(forecast_periods)))
  for (id in intersect(as.character(line_ids), names(pnl))) {
    for (p in forecast_periods) {
      i <- which(pnl$periode == p)
      if (!length(i)) next
      cur <- pnl[[id]][[i[[1]]]]
      if (is.finite(cur) && abs(cur) > 1e-6) next
      prev_p <- format(seq(as.Date(paste0(p, "-01")), by = "-1 year", length.out = 2)[[2]], "%Y-%m")
      j <- which(pnl$periode == prev_p)
      if (!length(j)) next
      pnl[[id]][[i[[1]]]] <- pnl[[id]][[j[[1]]]]
    }
  }
  fpa_pnl_derive(pnl)
}

#' Long monthly P&L with the most months in input/, mapped with the same plan.
#' The configured sheet is often one year. The seasonal rhythm needs the longer file.
fpa_history_pnl <- function(config, paths = fpa_paths(config)) {
  dir <- paths$input
  if (!dir.exists(dir)) return(NULL)
  files <- list.files(dir, pattern = "pnl.*\\.csv$", full.names = TRUE, ignore.case = TRUE)
  if (!length(files)) return(NULL)
  best <- NULL
  best_n <- 0L
  line_map <- fpa_line_map(config, "pnl")
  for (path in files) {
    df <- tryCatch(
      utils::read.csv(path, stringsAsFactors = FALSE, fileEncoding = "UTF-8", check.names = FALSE),
      error = function(e) NULL
    )
    if (is.null(df) || ncol(df) < 3L) next
    periode <- as.character(df[[2]])
    ok <- grepl("^[0-9]{4}-[0-9]{2}$", periode)
    if (sum(ok) < best_n) next
    long_df <- data.frame(
      libelle = as.character(df[[1]]),
      period_end = as.character(as.Date(paste0(periode, "-01"))),
      grain = "month",
      montant = suppressWarnings(as.numeric(df[[3]])),
      stringsAsFactors = FALSE
    )
    long_df <- long_df[ok & is.finite(long_df$montant), , drop = FALSE]
    mapped <- tryCatch(fpa_map_amounts(long_df, line_map), error = function(e) NULL)
    if (is.null(mapped) || !nrow(mapped)) next
    built <- fpa_build_pnl(mapped)
    n <- nrow(built)
    if (n > best_n) {
      best <- built
      best_n <- n
    }
  }
  best
}

#' Value of one open month from the seasonal ratio of up to 36 closed months.
#' Current year-to-date times the median (that month / same year-to-date) of prior years.
fpa_seasonal_month <- function(pnl, history, line, periode, actual_periods, lookback = 36L) {
  src <- if (!is.null(history) && nrow(history) && line %in% names(history)) history else pnl
  if (is.null(src) || !nrow(src) || !(line %in% names(src))) return(NA_real_)
  month <- as.integer(substr(periode, 6, 7))
  year <- as.integer(substr(periode, 1, 4))
  prev_same <- sprintf("%04d-%02d", year - 1L, month)
  # The statement month wins. A parallel history can disagree with the month on screen.
  copy_prev <- function() {
    if (prev_same %in% pnl$periode) {
      v <- suppressWarnings(as.numeric(pnl[[line]][match(prev_same, pnl$periode)]))
      if (length(v) == 1L && is.finite(v)) return(v)
    }
    if (prev_same %in% src$periode) {
      v <- suppressWarnings(as.numeric(src[[line]][match(prev_same, src$periode)]))
      if (length(v) == 1L && is.finite(v)) return(v)
    }
    NA_real_
  }
  # January has no YTD in the same year. Copy the same month last year.
  if (month < 2L) return(copy_prev())
  ytd <- sprintf("%04d-%02d", year, seq_len(month - 1L))
  ytd <- intersect(ytd, as.character(actual_periods))
  ytd <- ytd[ytd %in% as.character(pnl$periode)]
  if (!length(ytd)) return(copy_prev())
  ytd_m <- as.integer(substr(ytd, 6, 7))
  cut <- as.Date(paste0(periode, "-01"))
  keep <- as.Date(paste0(src$periode, "-01"))
  src <- src[!is.na(keep) & keep < cut & keep >= seq(cut, by = "-36 months", length.out = 2L)[[2]], , drop = FALSE]
  if (!nrow(src)) return(NA_real_)
  years <- sort(unique(as.integer(substr(src$periode, 1, 4))))
  years <- years[years < year]
  ratios <- numeric(0)
  for (y in years) {
    key_m <- sprintf("%04d-%02d", y, month)
    keys <- sprintf("%04d-%02d", y, ytd_m)
    if (!key_m %in% src$periode || !all(keys %in% src$periode)) next
    den <- sum(suppressWarnings(as.numeric(src[[line]][match(keys, src$periode)])), na.rm = TRUE)
    num <- suppressWarnings(as.numeric(src[[line]][match(key_m, src$periode)]))
    if (!is.finite(den) || abs(den) < 1 || !is.finite(num)) next
    ratios <- c(ratios, num / den)
  }
  cur <- sum(suppressWarnings(as.numeric(pnl[[line]][match(ytd, pnl$periode)])), na.rm = TRUE)
  if (!is.finite(cur)) return(NA_real_)
  if (length(ratios)) return(cur * stats::median(ratios))
  copy_prev()
}

#' Fill forecast months that are still empty. Closed months stay as imported.
#' Months past the first year copy a month already filled on the P&L.
fpa_pnl_apply_seasonal_level <- function(pnl, history, forecast_periods, actual_periods, line_ids) {
  if (is.null(pnl) || !nrow(pnl) || !length(forecast_periods) || !length(line_ids)) return(pnl)
  forecast_periods <- sort(unique(as.character(forecast_periods)))
  for (line in intersect(as.character(line_ids), names(pnl))) {
    for (p in forecast_periods) {
      i <- match(p, pnl$periode)
      if (is.na(i)) next
      cur <- suppressWarnings(as.numeric(pnl[[line]][[i]]))
      if (is.finite(cur) && abs(cur) > 1) next
      est <- fpa_seasonal_month(pnl, history, line, p, actual_periods)
      if (!is.finite(est)) {
        prev_p <- format(seq(as.Date(paste0(p, "-01")), by = "-1 year", length.out = 2)[[2]], "%Y-%m")
        j <- match(prev_p, pnl$periode)
        if (!is.na(j)) est <- suppressWarnings(as.numeric(pnl[[line]][[j]]))
      }
      if (is.finite(est)) pnl[[line]][[i]] <- est
    }
  }
  need <- c("REV", "OTHER_OP_INC", "COGS", "PAYROLL", "OPEX_OTHER", "D_A", "FIN", "EXCEP", "TAX")
  if (all(need %in% names(pnl))) return(fpa_pnl_derive(pnl))
  pnl
}

fpa_history_span <- function(history) {
  if (is.null(history) || !nrow(history)) return(list(n = 0L, from = "", to = ""))
  pers <- sort(unique(as.character(history$periode)))
  list(n = length(pers), from = pers[[1]], to = pers[[length(pers)]])
}

#' Standing lock: closed months keep the locked amounts. A new file cannot rewrite them.
fpa_apply_standing_lock <- function(pnl, paths) {
  lock_path <- file.path(paths$work, "standing-lock.csv")
  if (!file.exists(lock_path)) return(pnl)
  lock <- utils::read.csv(lock_path, stringsAsFactors = FALSE)
  if (!nrow(lock) || !all(c("periode", "line_id", "montant") %in% names(lock))) return(pnl)
  for (i in seq_len(nrow(lock))) {
    p <- as.character(lock$periode[[i]])
    id <- as.character(lock$line_id[[i]])
    if (!(id %in% names(pnl)) || !(p %in% pnl$periode)) next
    pnl[[id]][pnl$periode == p] <- as.numeric(lock$montant[[i]])
  }
  fpa_pnl_derive(pnl)
}
