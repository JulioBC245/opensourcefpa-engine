# SPDX-License-Identifier: AGPL-3.0-or-later
# Copyright (C) 2026 Julio Bauer Cantuaria
# fpa-phasing.R — Spread an annual budget over the twelve months of the year
# LAST CHANGE: 2026-09-21
#
# Grain honesty, the boundary that matters: actuals are never interpolated.
# A budget is a plan, not a fact, so spreading it is an ordinary planning act.
# Every phased row carries the method that produced it, and the annual total
# voted by the subscriber is preserved to the cent.

FPA_PHASE_MIN_MONTHS <- 24L
FPA_PHASE_MAX_YEARS <- 3L

#' Twelve month-end dates of the fiscal year ending at period_end.
fpa_fy_month_ends <- function(period_end) {
  pe <- as.Date(period_end)
  if (is.na(pe)) return(as.Date(character(0)))
  last_start <- as.Date(format(pe, "%Y-%m-01"))
  starts <- rev(seq(last_start, by = "-1 month", length.out = 12))
  out <- as.Date(rep(NA, 12))
  for (i in seq_along(starts)) {
    out[[i]] <- fpa_month_end(
      as.integer(format(starts[[i]], "%Y")),
      as.integer(format(starts[[i]], "%m"))
    )
  }
  out
}

#' Monthly shares of a line, from its own monthly actuals.
#'
#' Only complete years count, so a half-read year cannot tilt the profile.
#' Three years in median is the default: two is thin, and five describes a
#' company that has since changed. Returns NULL when the evidence is too thin,
#' which is the caller's signal to fall back to twelfths.
fpa_budget_season_on <- function(config) {
  if (is.null(config)) return(FALSE)
  s <- config$budget_season$shape_from_history
  if (!is.null(s)) return(isTRUE(s))
  u <- config$budget_phasing$use_history
  if (!is.null(u)) return(isTRUE(u))
  FALSE
}

fpa_budget_season_drop <- function(config) {
  if (is.null(config)) return(FALSE)
  s <- config$budget_season$drop_odd_months
  if (!is.null(s)) return(isTRUE(s))
  u <- config$budget_phasing$drop_odd_months
  if (!is.null(u)) return(isTRUE(u))
  FALSE
}

fpa_budget_season_lookback <- function(config) {
  n <- suppressWarnings(as.integer(config$budget_season$lookback_years %||% FPA_PHASE_MAX_YEARS))
  if (length(n) != 1L || !is.finite(n) || n < 1L) return(FPA_PHASE_MAX_YEARS)
  min(as.integer(n), 5L)
}

#' A monthly budget is "flat" when the months barely differ. A budget the
#' subscriber already shaped, month by month, is left alone.
fpa_budget_is_flat <- function(amounts) {
  x <- suppressWarnings(as.numeric(amounts))
  x <- x[is.finite(x)]
  if (length(x) < 6L) return(FALSE)
  m <- mean(abs(x))
  if (!is.finite(m) || m < 1) return(FALSE)
  (max(x) - min(x)) <= 0.02 * m
}

fpa_seasonal_shares <- function(monthly_mapped, line_id,
                                max_years = FPA_PHASE_MAX_YEARS,
                                min_months = FPA_PHASE_MIN_MONTHS,
                                keep_years = NULL,
                                drop_odd = FALSE) {
  if (is.null(monthly_mapped) || !nrow(monthly_mapped)) return(NULL)
  d <- monthly_mapped[monthly_mapped$functional_label == line_id, , drop = FALSE]
  if (!nrow(d) || nrow(d) < min_months) return(NULL)
  pe <- as.Date(d$period_end)
  d <- d[!is.na(pe), , drop = FALSE]
  pe <- pe[!is.na(pe)]
  if (!nrow(d)) return(NULL)
  d$year <- as.integer(format(pe, "%Y"))
  d$month <- as.integer(format(pe, "%m"))

  counts <- tapply(d$month, d$year, function(x) length(unique(x)))
  full <- as.integer(names(counts)[counts == 12L])
  if (!is.null(keep_years)) {
    want <- suppressWarnings(as.integer(keep_years))
    want <- want[is.finite(want)]
    full <- intersect(full, want)
  }
  # One named year is a shape the subscriber asked for. An unnamed search still needs two.
  if (length(full) < 1L) return(NULL)
  if (is.null(keep_years) && length(full) < 2L) return(NULL)
  full <- sort(full, decreasing = TRUE)
  if (is.null(keep_years)) full <- full[seq_len(min(max_years, length(full)))]

  shares <- matrix(NA_real_, nrow = length(full), ncol = 12L)
  for (i in seq_along(full)) {
    sl <- d[d$year == full[[i]], , drop = FALSE]
    total <- sum(sl$montant, na.rm = TRUE)
    if (!is.finite(total) || abs(total) < 1e-9) return(NULL)
    for (m in 1:12) {
      shares[i, m] <- sum(sl$montant[sl$month == m], na.rm = TRUE) / total
    }
  }

  dropped <- character(0)
  if (isTRUE(drop_odd) && nrow(shares) >= 3L) {
    for (m in seq_len(ncol(shares))) {
      x <- shares[, m]
      ok <- which(is.finite(x))
      if (length(ok) < 3L) next
      med <- stats::median(x[ok])
      if (!is.finite(med) || abs(med) < 1e-6) next
      odd <- ok[x[ok] > 2 * abs(med) | x[ok] < 0.5 * abs(med)]
      if (length(odd) && length(odd) < length(ok)) {
        dropped <- c(dropped, sprintf("%04d-%02d", full[odd], m))
        shares[odd, m] <- NA_real_
      }
    }
  }

  idx <- if (length(full) >= 3L) {
    apply(shares, 2, function(x) stats::median(x[is.finite(x)]))
  } else {
    apply(shares, 2, function(x) mean(x[is.finite(x)]))
  }
  if (any(!is.finite(idx))) {
    fill <- mean(idx[is.finite(idx)])
    if (!is.finite(fill)) return(NULL)
    idx[!is.finite(idx)] <- fill
  }
  s <- sum(idx, na.rm = TRUE)
  if (!is.finite(s) || abs(s) < 1e-9) return(NULL)
  out <- as.numeric(idx / s)
  attr(out, "dropped") <- unique(dropped)
  out
}

#' Closed years of revenue the subscriber can keep or set aside.
#' `apart` is a year whose total is less than half, or more than double, the others.
fpa_season_year_rows <- function(monthly_mapped, budget_year, lookback = FPA_PHASE_MAX_YEARS) {
  empty <- data.frame(
    year = character(), months = integer(), total = numeric(),
    apart = logical(), complete = logical(), stringsAsFactors = FALSE
  )
  if (is.null(monthly_mapped) || !nrow(monthly_mapped) || !"functional_label" %in% names(monthly_mapped)) return(empty)
  d <- monthly_mapped[as.character(monthly_mapped$functional_label) == "REV", , drop = FALSE]
  if (!nrow(d)) return(empty)
  pe <- as.Date(d$period_end)
  d <- d[!is.na(pe), , drop = FALSE]
  if (!nrow(d)) return(empty)
  d$year <- format(pe[!is.na(pe)], "%Y")
  d$month <- as.integer(format(pe[!is.na(pe)], "%m"))
  by <- as.character(budget_year)
  d <- d[d$year < by, , drop = FALSE]
  if (!nrow(d)) return(empty)
  years <- sort(unique(d$year), decreasing = TRUE)
  rows <- lapply(years, function(y) {
    sl <- d[d$year == y, , drop = FALSE]
    data.frame(
      year = y,
      months = length(unique(sl$month)),
      total = sum(sl$montant, na.rm = TRUE),
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, rows)
  out$complete <- out$months >= 12L
  keep <- out$year[out$complete]
  keep <- keep[seq_len(min(length(keep), lookback))]
  out <- out[out$year %in% keep | (!out$complete & out$year %in% years[seq_len(min(length(years), lookback + 1L))]), , drop = FALSE]
  med <- stats::median(out$total[out$complete])
  out$apart <- out$complete & is.finite(med) & abs(med) > 1 & (out$total > 2 * abs(med) | out$total < 0.5 * abs(med))
  out[order(out$year), , drop = FALSE]
}

#' Spread a flat monthly budget over the shape of the kept years.
#' A budget that already varies by month is returned unchanged: that shape was voted.
#' The annual total of each line is preserved. A missing block does nothing.
fpa_budget_reshape <- function(budget, monthly_mapped, config, keep_years = NULL) {
  info <- list(on = FALSE, years = character(0), dropped = character(0), fallback = character(0), apart = character(0))
  if (is.null(budget) || !nrow(budget)) {
    attr(budget, "season") <- info
    return(budget)
  }
  if (!fpa_budget_season_on(config)) {
    attr(budget, "season") <- info
    return(budget)
  }
  info$on <- TRUE
  look <- fpa_budget_season_lookback(config)
  drop <- fpa_budget_season_drop(config)
  byear <- max(substr(as.character(budget$periode), 1, 4))
  catalogue <- fpa_season_year_rows(monthly_mapped, byear, look)
  info$apart <- catalogue$year[catalogue$apart]
  if (is.null(keep_years)) keep_years <- catalogue$year[catalogue$complete]
  keep_years <- unique(as.character(keep_years))
  info$years <- keep_years
  if (length(keep_years) < 1L) {
    info$fallback <- "none"
    attr(budget, "season") <- info
    return(budget)
  }
  out <- budget
  rev_shares <- fpa_seasonal_shares(monthly_mapped, "REV", keep_years = keep_years, drop_odd = drop, min_months = 12L)
  if (!is.null(rev_shares)) info$dropped <- attr(rev_shares, "dropped") %||% character(0)
  lines <- unique(as.character(out$functional_label))
  for (line in lines) {
    idx <- which(as.character(out$functional_label) == line)
    per <- as.character(out$periode[idx])
    years_b <- unique(substr(per, 1, 4))
    for (yb in years_b) {
      w <- idx[substr(per, 1, 4) == yb]
      if (length(w) < 6L) next
      if (!fpa_budget_is_flat(out$montant[w])) next
      shares <- fpa_seasonal_shares(monthly_mapped, line, keep_years = keep_years, drop_odd = drop, min_months = 12L)
      used <- line
      if (is.null(shares)) {
        shares <- rev_shares
        used <- "REV"
      }
      if (is.null(shares)) {
        info$fallback <- c(info$fallback, line)
        next
      }
      if (!identical(used, line)) info$fallback <- c(info$fallback, line)
      total <- sum(suppressWarnings(as.numeric(out$montant[w])), na.rm = TRUE)
      amounts <- fpa_apply_shares(total, shares)
      for (j in w) {
        m <- as.integer(substr(out$periode[[j]], 6, 7))
        if (is.finite(m) && m >= 1L && m <= 12L) out$montant[[j]] <- amounts[[m]]
      }
      if ("method" %in% names(out)) out$method[w] <- "seasonal_index"
    }
  }
  info$fallback <- unique(info$fallback)
  attr(out, "season") <- info
  out
}

#' Move an already-built variance onto a reshaped budget. Named pieces stay.
#' The gap moves into the part still to explain, and the annual pieces still add up.
fpa_variance_rebudget <- function(var_df, budget) {
  if (is.null(var_df) || !nrow(var_df) || is.null(budget) || !nrow(budget)) return(var_df)
  info <- attr(budget, "season")
  if (is.null(info) || !isTRUE(info$on)) return(var_df)
  fb <- as.character(info$fallback %||% character(0))
  if (length(fb) == 1L && fb %in% c("none", "need_two")) return(var_df)
  out <- var_df
  named <- intersect(c("price", "volume", "mix", "rate", "headcount"), names(out))
  for (i in seq_len(nrow(out))) {
    hit <- which(as.character(budget$functional_label) == as.character(out$line_id[[i]]) &
                   as.character(budget$periode) == as.character(out$periode[[i]]))
    if (!length(hit)) next
    bud <- suppressWarnings(as.numeric(budget$montant[[hit[[1]]]]))
    old <- suppressWarnings(as.numeric(out$budget[[i]]))
    if (!is.finite(bud) || !is.finite(old) || abs(bud - old) < 0.5) next
    out$budget[[i]] <- bud
    out$variance[[i]] <- suppressWarnings(as.numeric(out$variance[[i]])) - (bud - old)
    out$variance_pct[[i]] <- if (abs(bud) > 1e-9) out$variance[[i]] / bud else NA_real_
    if ("unexplained" %in% names(out)) {
      pieces <- sum(vapply(named, function(nm) {
        v <- suppressWarnings(as.numeric(out[[nm]][[i]]))
        if (is.finite(v)) v else 0
      }, numeric(1)))
      out$unexplained[[i]] <- out$variance[[i]] - pieces
    }
  }
  out
}

fpa_season_mapped <- function(config, paths) {
  hist <- tryCatch(fpa_line_history(config, paths), error = function(e) NULL)
  map <- tryCatch(fpa_line_map(config, "pnl"), error = function(e) NULL)
  if (is.null(hist) || !nrow(hist) || is.null(map)) return(NULL)
  long <- data.frame(
    libelle = as.character(hist$libelle),
    period_end = as.character(as.Date(paste0(as.character(hist$periode), "-01"))),
    grain = "month",
    montant = suppressWarnings(as.numeric(hist$montant)),
    stringsAsFactors = FALSE
  )
  long <- long[!is.na(as.Date(long$period_end)) & is.finite(long$montant), , drop = FALSE]
  mapped <- tryCatch(fpa_map_amounts(long, map), error = function(e) NULL)
  if (is.null(mapped) || !nrow(mapped)) return(NULL)
  data.frame(
    functional_label = as.character(mapped$functional_label),
    period_end = as.character(mapped$period_end),
    montant = as.numeric(mapped$montant),
    stringsAsFactors = FALSE
  )
}

fpa_season_keep_path <- function(paths) file.path(paths$work, "budget-season-keep.csv")

fpa_season_keep_read <- function(paths) {
  f <- fpa_season_keep_path(paths)
  if (!file.exists(f)) return(NULL)
  d <- tryCatch(utils::read.csv(f, stringsAsFactors = FALSE), error = function(e) NULL)
  if (is.null(d) || !all(c("year", "keep") %in% names(d))) return(NULL)
  as.character(d$year[suppressWarnings(as.integer(d$keep)) == 1L])
}

fpa_season_keep_write <- function(paths, years, keep) {
  if (is.null(paths) || !nzchar(paths$work %||% "")) return(invisible(NULL))
  dir.create(paths$work, recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(
    data.frame(year = as.character(years), keep = as.integer(as.character(years) %in% as.character(keep)), stringsAsFactors = FALSE),
    fpa_season_keep_path(paths),
    row.names = FALSE
  )
}

fpa_even_shares <- function() rep(1 / 12, 12)

#' Apply shares to an annual amount, preserving the total exactly.
#'
#' The residual from rounding lands in the largest month, where it is
#' proportionally smallest. The twelve months always sum to the voted figure,
#' which is the first thing anyone checks.
fpa_apply_shares <- function(annual_amount, shares, digits = 2L) {
  if (is.null(shares) || length(shares) != 12L) shares <- fpa_even_shares()
  raw <- as.numeric(annual_amount) * as.numeric(shares)
  out <- round(raw, digits)
  resid <- round(as.numeric(annual_amount) - sum(out), digits)
  if (is.finite(resid) && abs(resid) > 0) {
    k <- which.max(abs(out))
    if (length(k) == 1L) out[[k]] <- out[[k]] + resid
  }
  out
}

#' Normalise config$budget_phasing$rows into a named list of methods.
fpa_phasing_rows <- function(config) {
  plan <- config$budget_phasing
  if (is.null(plan)) return(list())
  rows <- plan$rows %||% list()
  out <- list()
  for (r in rows) {
    id <- as.character(r$line_id %||% "")
    if (!nzchar(id)) next
    out[[id]] <- list(
      method = as.character(r$method %||% "even"),
      follow = if (is.null(r$follow)) NA_character_ else as.character(r$follow),
      evidenced = isTRUE(r$evidenced)
    )
  }
  out
}

fpa_phasing_shape <- function(config) {
  as.character((config$budget_phasing$budget_shape) %||% "absent")
}

#' Spread an annual budget into monthly rows.
#'
#' @param budget_mapped functional_label / period_end / grain / montant
#' @param monthly_mapped the same shape, from the monthly income statement
#' @param config the loaded fpa-config.json
#' @return long data.frame: periode, functional_label, montant, method, evidenced
fpa_phase_budget <- function(budget_mapped, monthly_mapped, config) {
  empty <- data.frame(
    periode = character(), functional_label = character(),
    montant = numeric(), method = character(), evidenced = logical(),
    stringsAsFactors = FALSE
  )
  if (is.null(budget_mapped) || !nrow(budget_mapped)) return(empty)

  plan <- fpa_phasing_rows(config)
  # Anchor on the latest declared budget period: that is the fiscal year voted.
  pe_all <- as.Date(budget_mapped$period_end)
  pe_all <- pe_all[!is.na(pe_all)]
  if (!length(pe_all)) return(empty)
  anchor <- max(pe_all)
  months <- fpa_fy_month_ends(anchor)
  if (!length(months)) return(empty)
  month_keys <- format(months, "%Y-%m")

  slice <- budget_mapped[
    !is.na(as.Date(budget_mapped$period_end)) &
      as.Date(budget_mapped$period_end) == anchor, , drop = FALSE
  ]
  if (!nrow(slice)) return(empty)

  # Revenue first: a line that follows revenue needs the revenue profile.
  ids <- unique(as.character(slice$functional_label))
  ids <- c(intersect("REV", ids), setdiff(ids, "REV"))
  shares_cache <- list()
  rows <- list()

  for (id in ids) {
    total <- sum(slice$montant[slice$functional_label == id], na.rm = TRUE)
    spec <- plan[[id]] %||% list(method = "even", follow = NA_character_, evidenced = FALSE)
    method <- as.character(spec$method %||% "even")
    shares <- NULL

    if (identical(method, "seasonal_index") || identical(method, "driver")) {
      shares <- fpa_seasonal_shares(monthly_mapped, id, drop_odd = fpa_budget_season_drop(config))
      if (is.null(shares)) method <- "even"
    } else if (identical(method, "follow_line")) {
      target <- as.character(spec$follow %||% "REV")
      shares <- shares_cache[[target]]
      if (is.null(shares)) shares <- fpa_seasonal_shares(monthly_mapped, target, drop_odd = fpa_budget_season_drop(config))
      if (is.null(shares)) method <- "even"
    }
    if (is.null(shares)) shares <- fpa_even_shares()
    shares_cache[[id]] <- shares

    amounts <- fpa_apply_shares(total, shares)
    rows[[length(rows) + 1]] <- data.frame(
      periode = month_keys,
      functional_label = id,
      montant = amounts,
      method = method,
      evidenced = !identical(method, "even") || isTRUE(spec$evidenced),
      stringsAsFactors = FALSE
    )
  }

  if (!length(rows)) return(empty)
  do.call(rbind, rows)
}

#' Monthly budget ready for variance: phased when annual, passed through when
#' the subscriber already budgets by month.
fpa_budget_monthly <- function(reads, config) {
  b <- reads$budget
  if (is.null(b) || !isTRUE(b$ok) || is.null(b$mapped) || !nrow(b$mapped)) {
    return(NULL)
  }
  monthly_mapped <- if (!is.null(reads$pnl_monthly) && isTRUE(reads$pnl_monthly$ok)) {
    reads$pnl_monthly$mapped
  } else if (!is.null(reads$pnl_actuals) && isTRUE(reads$pnl_actuals$ok) &&
             identical(reads$pnl_actuals$grain, "month")) {
    reads$pnl_actuals$mapped
  } else {
    NULL
  }

  if (identical(b$grain, "month")) {
    m <- b$mapped
    return(data.frame(
      periode = format(as.Date(m$period_end), "%Y-%m"),
      functional_label = as.character(m$functional_label),
      montant = as.numeric(m$montant),
      method = "as_given",
      evidenced = TRUE,
      stringsAsFactors = FALSE
    ))
  }

  fpa_phase_budget(b$mapped, monthly_mapped, config)
}
