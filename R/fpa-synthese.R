# SPDX-License-Identifier: AGPL-3.0-or-later
# Copyright (C) 2026 Julio Bauer Cantuaria
# fpa-synthese.R — What the summary page is allowed to say.
# LAST CHANGE: 2026-10-11
# Realized months and forecast months stay apart. Unknown status stays unknown.
# One presentation model feeds the decision page: finding, cards, cash, evidence.

fpa_synthese_eur <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  if (!length(x) || !is.finite(x[[1]])) return("n/a")
  format(round(x[[1]]), big.mark = " ", scientific = FALSE, trim = TRUE)
}

#' Coarsest 1-2-2.5-5-10 step that keeps at most `max_ticks` labelled ticks.
#' Rounding the step up is what stops "30,000 / 32,500 / 35,000" from stacking.
fpa_shiny_y_step <- function(span, max_ticks = 6) {
  span <- suppressWarnings(as.numeric(span))
  if (length(span) != 1L || !is.finite(span) || span <= 0) return(1)
  max_ticks <- max(2, as.numeric(max_ticks))
  rough <- span / max_ticks
  mag <- 10^floor(log10(max(rough, 1e-9)))
  cand <- c(1, 2, 2.5, 5, 10) * mag
  ok <- cand[cand + 1e-9 >= rough]
  dtick <- if (length(ok)) min(ok) else max(cand)
  if (!is.finite(dtick) || dtick <= 0) dtick <- max(span / max_ticks, 1)
  dtick
}

#' Y-axis for Synthèse charts: fit from series low (default) or force through 0.
fpa_shiny_y_axis <- function(vals, mode = c("fit", "zero"), title = "EUR",
                             anchors = numeric(), pad = 0.08) {
  mode <- match.arg(mode)
  vals <- suppressWarnings(as.numeric(vals))
  vals <- vals[is.finite(vals)]
  anchors <- suppressWarnings(as.numeric(anchors))
  anchors <- anchors[is.finite(anchors)]
  base <- list(
    title = title,
    tickformat = ",d",
    separatethousands = TRUE,
    tickfont = list(size = 12, color = "#3d4c54"),
    zeroline = TRUE,
    zerolinecolor = "#9aadb6",
    zerolinewidth = 1,
    showgrid = TRUE,
    gridcolor = "#c5d0d6",
    gridwidth = 1,
    showline = FALSE,
    automargin = TRUE
  )
  if (!length(vals)) return(base)
  ymin <- min(vals)
  ymax <- max(vals)
  if (identical(mode, "zero")) {
    ymin <- min(0, ymin, anchors)
    ymax <- max(0, ymax, anchors)
  } else {
    span0 <- max(ymax - ymin, 1)
    near <- anchors[anchors >= ymin - 0.25 * span0 & anchors <= ymax + 0.25 * span0]
    if (length(near)) {
      ymin <- min(ymin, near)
      ymax <- max(ymax, near)
    }
    span0 <- max(ymax - ymin, 1)
    ymin <- ymin - pad * span0
    ymax <- ymax + pad * span0
  }
  if (!is.finite(ymin) || !is.finite(ymax) || ymax <= ymin) ymax <- ymin + 1
  span <- ymax - ymin
  dtick <- fpa_shiny_y_step(span, 6)
  ymin <- floor(ymin / dtick) * dtick
  ymax <- ceiling(ymax / dtick) * dtick
  if (identical(mode, "zero") && ymin > 0) ymin <- 0
  if (identical(mode, "zero") && ymax < 0) ymax <- 0
  # Snapping to the step can add a tick. Coarsen once if that happens.
  if ((ymax - ymin) / dtick > 6) {
    dtick <- fpa_shiny_y_step(ymax - ymin, 5)
    if ((ymax - ymin) / dtick > 6) dtick <- dtick * 2
    ymin <- floor(ymin / dtick) * dtick
    ymax <- ceiling(ymax / dtick) * dtick
    if (identical(mode, "zero") && ymin > 0) ymin <- 0
    if (identical(mode, "zero") && ymax < 0) ymax <- 0
  }
  base$range <- c(ymin, ymax)
  base$dtick <- dtick
  base$tick0 <- ymin
  base$minor <- list(
    tickmode = "linear",
    dtick = dtick / 2,
    showgrid = TRUE,
    gridcolor = "#e8eef1",
    gridwidth = 1,
    ticklen = 0
  )
  base
}

fpa_synthese_month_back <- function(periode, months = 12L) {
  d <- as.Date(paste0(as.character(periode), "-01"))
  format(seq(d, by = paste0("-", as.integer(months), " months"), length.out = 2L)[2], "%Y-%m")
}

#' One row per P&L month, with réalisé / prévision / inconnu and the revenue budget gap.
fpa_synthese_frame <- function(pnl, status = NULL, variance = NULL) {
  if (is.null(pnl) || !nrow(pnl)) {
    return(data.frame())
  }
  out <- pnl
  out$periode <- as.character(out$periode)
  out$etat <- rep("inconnu", nrow(out))
  if (!is.null(status) && nrow(status) && all(c("periode", "status") %in% names(status))) {
    m <- match(out$periode, as.character(status$periode))
    hit <- !is.na(m)
    flag <- as.character(status$status)[m[hit]]
    out$etat[hit] <- ifelse(
      flag == "forecast", "prévision",
      ifelse(flag %in% c("actual", "réalisé", "closed"), "réalisé", "inconnu")
    )
  }
  out$budget_rev <- NA_real_
  out$gap_rev <- NA_real_
  if (!is.null(variance) && nrow(variance) && all(c("periode", "line_id", "budget", "variance") %in% names(variance))) {
    rev <- variance[as.character(variance$line_id) == "REV", , drop = FALSE]
    m <- match(out$periode, as.character(rev$periode))
    hit <- !is.na(m)
    out$budget_rev[hit] <- suppressWarnings(as.numeric(rev$budget[m[hit]]))
    out$gap_rev[hit] <- suppressWarnings(as.numeric(rev$variance[m[hit]]))
  }
  out
}

fpa_synthese_hover_pnl <- function(frame) {
  if (is.null(frame) || !nrow(frame)) return(character())
  rev <- suppressWarnings(as.numeric(frame$REV))
  oi <- suppressWarnings(as.numeric(frame$OI))
  vapply(seq_len(nrow(frame)), function(i) {
    bits <- c(
      paste0(frame$periode[[i]], " · ", frame$etat[[i]]),
      paste0("Chiffre d'affaires ", fpa_synthese_eur(rev[[i]]), " EUR")
    )
    if (i > 1L && is.finite(rev[[i]]) && is.finite(rev[[i - 1L]])) {
      d <- rev[[i]] - rev[[i - 1L]]
      bits <- c(bits, paste0(fpa_txt("loc_prior_month"), if (d >= 0) "+" else "", fpa_synthese_eur(d), " EUR"))
    }
    ago <- fpa_synthese_month_back(frame$periode[[i]])
    j <- match(ago, frame$periode)
    if (!is.na(j) && is.finite(rev[[i]]) && is.finite(rev[[j]])) {
      d <- rev[[i]] - rev[[j]]
      bits <- c(bits, paste0(fpa_txt("loc_same_month_ly"), if (d >= 0) "+" else "", fpa_synthese_eur(d), " EUR"))
    }
    if (is.finite(rev[[i]]) && abs(rev[[i]]) > 0.5 && is.finite(oi[[i]])) {
      bits <- c(bits, paste0(
        fpa_txt("loc_oi_sp"), fpa_synthese_eur(oi[[i]]), " EUR, ",
        format(round(100 * oi[[i]] / rev[[i]], 1), nsmall = 1), " % du chiffre d'affaires"
      ))
    }
    if (is.finite(frame$budget_rev[[i]]) && is.finite(frame$gap_rev[[i]])) {
      bits <- c(bits, paste0(
        "Budget ", fpa_synthese_eur(frame$budget_rev[[i]]), fpa_txt("loc_eur_gap_sp"),
        if (frame$gap_rev[[i]] >= 0) "+" else "", fpa_synthese_eur(frame$gap_rev[[i]]), " EUR"
      ))
    }
    paste(bits, collapse = "<br>")
  }, character(1))
}

fpa_synthese_hover_cash <- function(cash, status = NULL) {
  if (is.null(cash) || !nrow(cash)) return(character())
  etat <- rep("reconstruit", nrow(cash))
  if (!is.null(status) && nrow(status) && all(c("periode", "status") %in% names(status))) {
    m <- match(as.character(cash$periode), as.character(status$periode))
    hit <- !is.na(m)
    flag <- as.character(status$status)[m[hit]]
    etat[hit] <- ifelse(flag == "forecast", "prévision", "reconstruit")
  }
  close_v <- suppressWarnings(as.numeric(cash$cash_close))
  open_v <- suppressWarnings(as.numeric(cash$cash_open))
  delta <- suppressWarnings(as.numeric(cash$delta_cash))
  vapply(seq_len(nrow(cash)), function(i) {
    paste(c(
      paste0(cash$periode[[i]], " · ", etat[[i]]),
      paste0(fpa_txt("loc_closing_sp"), fpa_synthese_eur(close_v[[i]]), " EUR"),
      paste0("Ouverture ", fpa_synthese_eur(open_v[[i]]), " EUR"),
      paste0("Variation ", if (is.finite(delta[[i]]) && delta[[i]] >= 0) "+" else "", fpa_synthese_eur(delta[[i]]), " EUR")
    ), collapse = "<br>")
  }, character(1))
}

#' Narrow the open month's scenario gap by the days still ahead, and carry that
#' narrower gap through later months. The edges stay one line, not a pinch
#' followed by the full gap.
fpa_shiny_band_remaining <- function(periode, central, low, high, today = Sys.Date()) {
  periode <- as.character(periode)
  central <- suppressWarnings(as.numeric(central))
  low <- suppressWarnings(as.numeric(low))
  high <- suppressWarnings(as.numeric(high))
  n <- length(periode)
  empty <- data.frame(
    periode = periode, central = central, low = low, high = high,
    left_frac = 1, stringsAsFactors = FALSE
  )
  if (!n || length(central) != n || length(low) != n || length(high) != n) return(empty)
  today <- as.Date(today)
  if (is.na(today)) return(empty)
  cur <- format(today, "%Y-%m")
  i <- match(cur, periode)
  if (is.na(i)) return(empty)
  start <- as.Date(paste0(cur, "-01"))
  end <- seq(start, by = "month", length.out = 2L)[[2]] - 1
  days <- as.numeric(end - start) + 1
  left <- as.numeric(end - today)
  frac <- if (is.finite(days) && days > 0) max(0, min(1, left / days)) else 1
  dlo <- low - central
  dhi <- high - central
  dlo[!is.finite(dlo)] <- 0
  dhi[!is.finite(dhi)] <- 0
  inc_lo <- if (n == 1L) dlo else c(dlo[[1]], diff(dlo))
  inc_hi <- if (n == 1L) dhi else c(dhi[[1]], diff(dhi))
  inc_lo[[i]] <- inc_lo[[i]] * frac
  inc_hi[[i]] <- inc_hi[[i]] * frac
  empty$low <- central + cumsum(inc_lo)
  empty$high <- central + cumsum(inc_hi)
  empty$left_frac <- frac
  empty
}

#' Numbers and sentences for the summary. Forecast months are not added into the realized totals.
fpa_synthese_brief <- function(pnl, cash = NULL, status = NULL, variance = NULL) {
  frame <- fpa_synthese_frame(pnl, status, variance)
  empty <- list(
    n_actual = 0L, n_forecast = 0L, last_actual = "", first_forecast = "",
    rev_actual = NA_real_, rev_forecast = NA_real_, oi_actual = NA_real_,
    margin = NA_real_, peak_month = "", peak_rev = NA_real_,
    cash_close = NA_real_, cash_close_month = "", cash_year_end = NA_real_,
    cash_year_end_month = "", cash_low = NA_real_, cash_low_month = "", cash_under = 0L,
    what = "", why = "", so = "", trend = "", cash_note = ""
  )
  if (!nrow(frame)) return(empty)
  actual <- frame$etat == "réalisé"
  forecast <- frame$etat == "prévision"
  rev <- suppressWarnings(as.numeric(frame$REV))
  oi <- suppressWarnings(as.numeric(frame$OI))
  rev_a <- sum(rev[actual], na.rm = TRUE)
  oi_a <- sum(oi[actual], na.rm = TRUE)
  peak_i <- if (any(actual & is.finite(rev))) which.max(ifelse(actual, rev, -Inf)) else NA_integer_
  last_a <- if (any(actual)) frame$periode[max(which(actual))] else ""
  first_f <- if (any(forecast)) frame$periode[which(forecast)[1]] else ""
  picks <- tryCatch(fpa_cash_period_picks(cash, status), error = function(e) NULL)
  close_v <- if (!is.null(cash) && nrow(cash)) suppressWarnings(as.numeric(cash$cash_close)) else numeric()
  low_i <- if (length(close_v) && any(is.finite(close_v))) which.min(close_v) else NA_integer_
  under <- if (!is.null(picks)) picks$under else if (length(close_v)) sum(close_v < 0, na.rm = TRUE) else 0L
  cash_closed <- if (!is.null(picks) && is.finite(picks$closed)) picks$closed else if (length(close_v)) {
    # Prefer last actual month when status is present.
    if (nzchar(last_a) && last_a %in% as.character(cash$periode)) {
      suppressWarnings(as.numeric(cash$cash_close[match(last_a, as.character(cash$periode))]))
    } else {
      tail(close_v, 1)
    }
  } else {
    NA_real_
  }
  cash_closed_month <- if (!is.null(picks) && nzchar(picks$closed_month)) picks$closed_month else last_a
  cash_year_end <- if (!is.null(picks) && is.finite(picks$year_end)) picks$year_end else if (length(close_v)) tail(close_v, 1) else NA_real_
  cash_year_end_month <- if (!is.null(picks) && nzchar(picks$year_end_month)) picks$year_end_month else if (length(close_v)) as.character(cash$periode[[length(close_v)]]) else ""
  cash_low <- if (!is.null(picks) && is.finite(picks$low)) picks$low else if (!is.na(low_i)) close_v[[low_i]] else NA_real_
  cash_low_month <- if (!is.null(picks) && nzchar(picks$low_month)) picks$low_month else if (!is.na(low_i)) as.character(cash$periode[[low_i]]) else ""
  cash_note <- if (!length(close_v) || !is.finite(cash_closed)) {
    fpa_txt("synth_cash_none")
  } else {
    paste(
      fpa_txt("synth_cash_what", fpa_synthese_eur(cash_closed)),
      if (is.finite(cash_year_end) && nzchar(cash_year_end_month) && !identical(cash_year_end_month, cash_closed_month)) {
        fpa_txt("synth_cash_fcst", cash_year_end_month, fpa_synthese_eur(cash_year_end))
      } else {
        ""
      },
      fpa_txt("synth_cash_why", cash_low_month, fpa_synthese_eur(cash_low)),
      if (under > 0L) fpa_txt("synth_cash_so_under", under) else fpa_txt("synth_cash_so_ok"),
      sep = " "
    )
  }
  what <- if (!is.na(peak_i)) {
    fpa_txt("synth_what", frame$periode[[peak_i]], fpa_synthese_eur(rev[[peak_i]]))
  } else {
    fpa_txt("synth_what_none")
  }
  why <- fpa_txt("synth_why")
  so <- if (nzchar(first_f)) fpa_txt("synth_so", first_f) else fpa_txt("synth_so_none")
  trend <- paste(what, why, so, sep = " ")
  list(
    n_actual = sum(actual),
    n_forecast = sum(forecast),
    last_actual = last_a,
    first_forecast = first_f,
    rev_actual = rev_a,
    rev_forecast = sum(rev[forecast], na.rm = TRUE),
    oi_actual = oi_a,
    margin = if (abs(rev_a) > 0.5) oi_a / rev_a else NA_real_,
    peak_month = if (!is.na(peak_i)) frame$periode[[peak_i]] else "",
    peak_rev = if (!is.na(peak_i)) rev[[peak_i]] else NA_real_,
    cash_close = cash_closed,
    cash_close_month = cash_closed_month,
    cash_year_end = cash_year_end,
    cash_year_end_month = cash_year_end_month,
    cash_low = cash_low,
    cash_low_month = cash_low_month,
    cash_under = as.integer(under),
    what = what,
    why = why,
    so = so,
    trend = trend,
    cash_note = cash_note,
    frame = frame
  )
}

#' Margin this period versus budget margin and versus same month last year when both exist.
fpa_synthese_margin_note <- function(pnl, status = NULL, variance = NULL) {
  frame <- fpa_synthese_frame(pnl, status, variance)
  if (!nrow(frame)) return("")
  actual <- frame$etat == "réalisé"
  if (!any(actual)) return("")
  i <- max(which(actual))
  rev <- suppressWarnings(as.numeric(frame$REV[[i]]))
  oi <- suppressWarnings(as.numeric(frame$OI[[i]]))
  margin <- if (is.finite(rev) && abs(rev) > 0.5 && is.finite(oi)) oi / rev else NA_real_
  margin_txt <- if (is.finite(margin)) paste0(format(round(100 * margin, 1), nsmall = 1), " %") else "n/a"
  bud_txt <- "n/a"
  has_budget <- FALSE
  if (!is.null(variance) && nrow(variance) && all(c("periode", "line_id", "budget", "actual") %in% names(variance))) {
    p <- as.character(frame$periode[[i]])
    rev_b <- variance[as.character(variance$line_id) == "REV" & as.character(variance$periode) == p, , drop = FALSE]
    oi_rows <- variance[as.character(variance$periode) == p, , drop = FALSE]
    if (nrow(rev_b) && is.finite(rev_b$budget[[1]]) && abs(rev_b$budget[[1]]) > 0.5) {
      # Budget OI from signed NI contribution when OI itself is not a variance line.
      oi_b <- NA_real_
      if ("OI" %in% as.character(oi_rows$line_id)) {
        oi_b <- suppressWarnings(as.numeric(oi_rows$budget[as.character(oi_rows$line_id) == "OI"][[1]]))
      }
      if (is.finite(oi_b)) {
        bud_m <- oi_b / rev_b$budget[[1]]
        bud_txt <- paste0(format(round(100 * bud_m, 1), nsmall = 1), " %")
        has_budget <- TRUE
      }
    }
  }
  ago <- fpa_synthese_month_back(frame$periode[[i]])
  j <- match(ago, frame$periode)
  py_txt <- "n/a"
  has_py <- FALSE
  if (!is.na(j)) {
    rev_py <- suppressWarnings(as.numeric(frame$REV[[j]]))
    oi_py <- suppressWarnings(as.numeric(frame$OI[[j]]))
    if (is.finite(rev_py) && abs(rev_py) > 0.5 && is.finite(oi_py)) {
      py_txt <- paste0(format(round(100 * oi_py / rev_py, 1), nsmall = 1), " %")
      has_py <- TRUE
    }
  }
  if (has_budget && has_py) {
    fpa_txt("synth_margin", margin_txt, bud_txt, py_txt)
  } else if (has_budget) {
    fpa_txt("synth_margin_no_py", margin_txt, bud_txt)
  } else if (has_py) {
    fpa_txt("synth_margin_py_only", margin_txt, py_txt)
  } else {
    fpa_txt("synth_margin_no_budget", margin_txt)
  }
}

#' Largest material move in the visible P&L window versus budget, else versus prior month.
fpa_synthese_pnl_headline <- function(pnl, variance = NULL, span = "12", start = NULL, abs_floor = 1000) {
  if (is.null(pnl) || !nrow(pnl)) return(fpa_txt("pnl_headline_none"))
  pers <- as.character(pnl$periode)
  n <- suppressWarnings(as.integer(span)[1])
  if (length(n) != 1L || is.na(n) || !is.finite(n) || n < 1L) n <- length(pers)
  start_txt <- as.character(start %||% "")[1]
  if (is.na(start_txt)) start_txt <- ""
  i0 <- match(start_txt, pers)
  if (length(i0) != 1L || is.na(i0)) i0 <- max(1L, length(pers) - n + 1L)
  i1 <- min(length(pers), i0 + n - 1L)
  if (!is.finite(i0) || !is.finite(i1) || i1 < i0) return(fpa_txt("pnl_headline_none"))
  win <- pers[i0:i1]
  lines <- intersect(c("REV", "COGS", "PAYROLL", "OPEX_OTHER", "OI", "NI"), names(pnl))
  best_abs <- 0
  best <- NULL
  if (!is.null(variance) && nrow(variance)) {
    sl <- variance[as.character(variance$periode) %in% win & as.character(variance$line_id) %in% lines, , drop = FALSE]
    if (nrow(sl)) {
      for (i in seq_len(nrow(sl))) {
        s <- fpa_variance_signed(sl$line_id[[i]], sl$variance[[i]])
        if (!is.finite(s) || abs(s) <= abs_floor) next
        if (abs(s) > best_abs) {
          best_abs <- abs(s)
          best <- list(kind = "budget", line = sl$line_id[[i]], periode = sl$periode[[i]], signed = s)
        }
      }
    }
  }
  if (is.null(best) && length(win) >= 2L) {
    for (ln in lines) {
      vals <- suppressWarnings(as.numeric(pnl[[ln]][match(win, pers)]))
      for (k in seq_along(win)[-1L]) {
        if (!is.finite(vals[[k]]) || !is.finite(vals[[k - 1L]])) next
        d <- fpa_variance_signed(ln, vals[[k]] - vals[[k - 1L]])
        if (!is.finite(d) || abs(d) <= abs_floor) next
        if (abs(d) > best_abs) {
          best_abs <- abs(d)
          best <- list(kind = "mom", line = ln, periode = win[[k]], signed = d)
        }
      }
    }
  }
  if (is.null(best)) return(fpa_txt("pnl_headline_none"))
  label <- tryCatch(fpa_line_fr(best$line), error = function(e) best$line)
  if (identical(best$kind, "budget")) {
    # Engine variance is actual - budget; "above budget" follows the raw engine gap for income lines,
    # and the absolute amount for the sentence.
    raw <- best$signed / fpa_pnl_ni_sign(best$line)
    if (!is.finite(raw)) raw <- best$signed
    if (raw >= 0) {
      fpa_txt("pnl_headline", label, fpa_synthese_eur(abs(best$signed)), best$periode)
    } else {
      fpa_txt("pnl_headline_below", label, fpa_synthese_eur(abs(best$signed)), best$periode)
    }
  } else {
    fpa_txt("pnl_headline_mom", label, fpa_synthese_eur(best$signed), best$periode)
  }
}

#' Landing of one fiscal year. Budget comparator requires every fiscal month.
#' Closed-month pace is kept for optional detail only; it never drives a warning.
fpa_synthese_land <- function(frame, budget = NULL, cash_close = NA_real_, cash_year_end = NA_real_,
                              cash_year_end_month = "", cash_low = NA_real_, cash_low_month = "",
                              cash_under = 0L, fy_months = character()) {
  empty <- list(ok = FALSE)
  if (is.null(frame) || !nrow(frame) || !("etat" %in% names(frame))) return(empty)
  fy_months <- as.character(fy_months %||% character())
  fy_months <- fy_months[grepl("^[0-9]{4}-[0-9]{2}$", fy_months)]
  if (length(fy_months)) {
    frame <- frame[as.character(frame$periode) %in% fy_months, , drop = FALSE]
    if (!nrow(frame)) return(empty)
  }
  actual <- frame$etat == "réalisé"
  forecast <- frame$etat == "prévision"
  unknown <- !actual & !forecast
  rev <- suppressWarnings(as.numeric(frame$REV))
  oi <- suppressWarnings(as.numeric(frame$OI))
  n_a <- sum(actual)
  n_f <- sum(forecast)
  rev_a <- sum(rev[actual], na.rm = TRUE)
  rev_f <- sum(rev[forecast], na.rm = TRUE)
  oi_a <- sum(oi[actual], na.rm = TRUE)
  oi_f <- sum(oi[forecast], na.rm = TRUE)
  # Unknown months do not count as booked or forecast.
  rev_y <- rev_a + rev_f
  oi_y <- oi_a + oi_f
  year <- if (length(fy_months)) substr(fy_months[[length(fy_months)]], 1, 4) else ""
  if (!nzchar(year) && any(actual)) year <- substr(as.character(frame$periode[max(which(actual))]), 1, 4)
  if (!nzchar(year) && nrow(frame)) year <- substr(as.character(frame$periode[[1]]), 1, 4)
  cov <- fpa_synthese_budget_coverage(budget, if (length(fy_months)) fy_months else paste0(year, sprintf("-%02d", 1:12)), "REV")
  bud_oi <- fpa_synthese_budget_coverage(budget, if (length(fy_months)) fy_months else paste0(year, sprintf("-%02d", 1:12)), "OI")
  pace <- if (n_a > 0L && n_f > 0L) rev_a / n_a * n_f else NA_real_
  list(
    ok = TRUE,
    year = year,
    fy_months = fy_months,
    n_a = as.integer(n_a),
    n_f = as.integer(n_f),
    n_unknown = as.integer(sum(unknown)),
    rev_a = rev_a, rev_f = rev_f, rev_y = rev_y,
    oi_a = oi_a, oi_f = oi_f, oi_y = oi_y,
    booked_pct = if (abs(rev_y) > 0.5) 100 * rev_a / rev_y else NA_real_,
    pace = pace,
    pace_off = FALSE,
    margin_a = if (abs(rev_a) > 0.5) oi_a / rev_a else NA_real_,
    margin_y = if (abs(rev_y) > 0.5) oi_y / rev_y else NA_real_,
    bud_covers = isTRUE(cov$covers),
    bud_partial = isTRUE(cov$partial),
    bud_last = cov$last,
    bud_rev = cov$total,
    bud_oi = bud_oi$total,
    bud_oi_covers = isTRUE(bud_oi$covers),
    cash_close = suppressWarnings(as.numeric(cash_close))[1],
    cash_year_end = suppressWarnings(as.numeric(cash_year_end))[1],
    cash_year_end_month = as.character(cash_year_end_month %||% "")[1],
    cash_delta = {
      ye <- suppressWarnings(as.numeric(cash_year_end))[1]
      cl <- suppressWarnings(as.numeric(cash_close))[1]
      if (is.finite(ye) && is.finite(cl)) ye - cl else NA_real_
    },
    cash_low = suppressWarnings(as.numeric(cash_low))[1],
    cash_low_month = as.character(cash_low_month %||% "")[1],
    cash_under = as.integer(cash_under %||% 0L)[1]
  )
}

#' Full-year budget coverage: every fiscal month must carry a finite amount.
fpa_synthese_budget_coverage <- function(budget, fy_months, line = "REV") {
  out <- list(covers = FALSE, partial = FALSE, total = NA_real_, last = NA_character_, months = character())
  fy_months <- as.character(fy_months %||% character())
  fy_months <- fy_months[grepl("^[0-9]{4}-[0-9]{2}$", fy_months)]
  if (!length(fy_months) || is.null(budget) || !is.data.frame(budget) || !nrow(budget)) return(out)
  per <- as.character(budget$periode)
  vals <- NULL
  if (all(c("functional_label", "montant") %in% names(budget))) {
    hit <- as.character(budget$functional_label) == line
    per <- per[hit]
    vals <- suppressWarnings(as.numeric(budget$montant[hit]))
  } else if (line %in% names(budget)) {
    vals <- suppressWarnings(as.numeric(budget[[line]]))
  } else {
    return(out)
  }
  keep <- grepl("^[0-9]{4}-[0-9]{2}", per)
  per <- substr(per[keep], 1, 7)
  vals <- vals[keep]
  if (length(per)) out$last <- max(per)
  by_m <- tapply(vals, per, function(x) sum(x[is.finite(x)], na.rm = TRUE))
  present <- intersect(fy_months, names(by_m))
  out$months <- present
  if (!length(present)) return(out)
  out$partial <- length(present) < length(fy_months)
  out$covers <- length(present) == length(fy_months) && all(is.finite(as.numeric(by_m[present])))
  if (isTRUE(out$covers)) out$total <- sum(as.numeric(by_m[present]), na.rm = TRUE)
  out
}

#' Origin labels used on Synthèse.
fpa_synth_origin <- function(code) {
  switch(
    as.character(code %||% ""),
    actual = fpa_txt("synth_origin_actual"),
    rebuilt = fpa_txt("synth_origin_rebuilt"),
    forecast = fpa_txt("synth_origin_forecast"),
    plan = fpa_txt("synth_origin_plan"),
    declared = fpa_txt("synth_origin_declared"),
    move = fpa_txt("synth_origin_move"),
    unknown = fpa_txt("synth_origin_unknown"),
    ""
  )
}

#' Short-term outlook state from the typed bank file.
#' A date after today is planned, not an observed bank reading.
fpa_synth_outlook_state <- function(saved, roll = NULL, today = Sys.Date()) {
  today <- as.Date(today)
  if (is.null(saved)) {
    return(list(
      state = "need_balance", opening = NA_real_, as_of = NA_character_,
      age_days = NA_integer_, low = NA_real_, low_date = NA_character_,
      n_items = 0L, stale = FALSE, planned = FALSE
    ))
  }
  opening <- suppressWarnings(as.numeric(saved$opening %||% NA_real_))
  as_of <- fpa_outlook_date(saved$as_of)
  n_items <- length(saved$items %||% list())
  age <- if (is.finite(as_of) && is.finite(today)) as.integer(today - as_of) else NA_integer_
  planned <- is.finite(as_of) && is.finite(today) && as_of > today
  stale <- !isTRUE(planned) && is.finite(age) && age > 7L
  if (!is.finite(opening)) {
    state <- "need_balance"
  } else if (n_items < 1L) {
    state <- "balance_only"
  } else if (isTRUE(stale)) {
    state <- "stale_horizon"
  } else {
    state <- "partial"
  }
  low <- if (!is.null(roll) && is.finite(roll$low %||% NA_real_)) roll$low else NA_real_
  low_date <- if (!is.null(roll) && is.finite(roll$low_date %||% as.Date(NA))) {
    format(fpa_outlook_date(roll$low_date), "%Y-%m-%d")
  } else {
    NA_character_
  }
  list(
    state = state,
    opening = opening,
    as_of = if (is.finite(as_of)) as.character(as_of) else NA_character_,
    age_days = age,
    low = low,
    low_date = low_date,
    n_items = as.integer(n_items),
    stale = isTRUE(stale),
    planned = isTRUE(planned)
  )
}

#' Trust levels for cash / OI / revenue on the home page.
fpa_synth_trust <- function(L, outlook, cash_floor = 0, model_cash_at_asof = NA_real_) {
  reasons <- character()
  cash <- "limited"
  oi <- "ok"
  rev <- "ok"
  reasons <- c(reasons, fpa_txt("synth_trust_cash_rebuilt"))
  if (!is.null(outlook) && identical(outlook$state, "need_balance")) {
    reasons <- c(reasons, fpa_txt("synth_trust_need_balance"))
  }
  if (!is.null(outlook) && identical(outlook$state, "balance_only")) {
    reasons <- c(reasons, fpa_txt("synth_trust_balance_only"))
  }
  gap <- NA_real_
  if (!is.null(outlook) && is.finite(outlook$opening) && is.finite(model_cash_at_asof)) {
    gap <- outlook$opening - model_cash_at_asof
    thr <- fpa_synth_gap_thr(outlook$opening)
    if (abs(gap) > thr) {
      cash <- "limited"
      reasons <- c(reasons, fpa_txt("synth_trust_cash_gap", fpa_synthese_eur(abs(gap))))
    }
  }
  if (isTRUE(L$bud_partial) && !isTRUE(L$bud_covers)) {
    rev <- "limited"
    reasons <- c(reasons, fpa_txt("synth_trust_bud_partial"))
  }
  if (!isTRUE(L$bud_covers)) {
    if (!"limited" %in% rev) rev <- if (identical(rev, "ok")) "ok" else rev
  }
  if (is.finite(L$n_unknown) && L$n_unknown > 0L) {
    oi <- "limited"
    rev <- "limited"
    reasons <- c(reasons, fpa_txt("synth_trust_unknown_months", as.character(L$n_unknown)))
  }
  list(
    cash = cash, oi = oi, rev = rev, reasons = unique(reasons[nzchar(reasons)]),
    declared_model_gap = gap, floor = suppressWarnings(as.numeric(cash_floor))[1]
  )
}

#' Rule-based lead finding. First match wins.
fpa_synth_finding <- function(L, trust, outlook, cash_floor = 0, model_cash_at_asof = NA_real_,
                              cash_today = NA_real_, today = Sys.Date()) {
  eur <- fpa_synthese_eur
  floor <- suppressWarnings(as.numeric(cash_floor))[1]
  if (!is.finite(floor)) floor <- 0
  has_declared <- !is.null(outlook) && is.finite(outlook$opening)
  # 1. Missing bank balance — only when no live opening and no journal diamond.
  if (!is.null(outlook) && identical(outlook$state, "need_balance") && !has_declared) {
    body <- if (is.finite(cash_today)) {
      fpa_txt(
        "synth_find_need_bal_b",
        fpa_synth_date_fr(today),
        eur(cash_today),
        fpa_ym_fr(L$cash_year_end_month),
        eur(L$cash_year_end)
      )
    } else {
      fpa_txt("synth_find_need_bal_b_ye", eur(L$cash_year_end), fpa_ym_fr(L$cash_year_end_month))
    }
    return(list(
      id = "need_balance",
      severity = "watch",
      title = fpa_txt("synth_find_need_bal_t"),
      body = body,
      action_label = fpa_txt("synth_find_need_bal_a"),
      action_tab = "Trésorerie", action_open = "out", action_line = ""
    ))
  }
  # 2. Declared vs model gap when both exist (journal diamond or typed opening).
  gap <- trust$declared_model_gap
  thr <- fpa_synth_gap_thr(outlook$opening %||% 0)
  if (has_declared && is.finite(gap) && is.finite(model_cash_at_asof) && abs(gap) > thr) {
    return(list(
      id = "cash_gap",
      severity = "alert",
      title = fpa_txt("synth_find_gap_t"),
      body = fpa_txt(
        "synth_find_gap_b",
        eur(outlook$opening),
        format(fpa_outlook_date(outlook$as_of), "%d/%m/%Y"),
        eur(model_cash_at_asof),
        eur(abs(gap))
      ),
      action_label = fpa_txt("synth_find_gap_a"),
      action_tab = "Trésorerie", action_open = "out", action_line = ""
    ))
  }
  # 2b. A small gap is a quiet note, not a warning.
  if (has_declared && is.finite(gap) && is.finite(model_cash_at_asof) && abs(gap) >= 1) {
    return(list(
      id = "cash_gap_quiet",
      severity = "ok",
      title = fpa_txt("synth_find_gap_quiet_t"),
      body = fpa_txt(
        "synth_find_gap_quiet_b",
        eur(outlook$opening),
        format(fpa_outlook_date(outlook$as_of), "%d/%m/%Y"),
        eur(model_cash_at_asof),
        eur(abs(gap))
      ),
      action_label = fpa_txt("synth_find_gap_a"),
      action_tab = "Trésorerie", action_open = "out", action_line = ""
    ))
  }
  # 3. Declared balance incomplete (balance only, no dated items).
  if (!is.null(outlook) && identical(outlook$state, "balance_only")) {
    return(list(
      id = "balance_only",
      severity = "watch",
      title = fpa_txt("synth_find_bal_only_t"),
      body = fpa_txt(
        "synth_find_bal_only_b",
        eur(outlook$opening),
        format(fpa_outlook_date(outlook$as_of), "%d/%m/%Y")
      ),
      action_label = fpa_txt("synth_find_bal_only_a"),
      action_tab = "Trésorerie", action_open = "out", action_line = ""
    ))
  }
  # 4. Cash under floor (future or already).
  if (is.finite(L$cash_year_end) && L$cash_year_end < floor) {
    return(list(
      id = "cash_floor",
      severity = "alert",
      title = fpa_txt("synth_find_floor_t"),
      body = fpa_txt(
        "synth_find_floor_b",
        eur(L$cash_year_end),
        fpa_ym_fr(L$cash_year_end_month),
        eur(floor)
      ),
      action_label = fpa_txt("synth_find_floor_a"),
      action_tab = "Trésorerie", action_open = "", action_line = ""
    ))
  }
  # 5. Material OI erosion on open months.
  if (is.finite(L$oi_a) && L$oi_a > 0 && is.finite(L$oi_f) && L$oi_f < -0.5 && abs(L$oi_f) >= max(1000, 0.15 * L$oi_a)) {
    return(list(
      id = "oi_erosion",
      severity = "watch",
      title = fpa_txt("synth_find_oi_t"),
      body = fpa_txt("synth_find_oi_b", eur(L$oi_a), eur(abs(L$oi_f)), eur(L$oi_y)),
      action_label = fpa_txt("synth_find_oi_a"),
      action_tab = "Scénarios", action_open = "", action_line = "OI"
    ))
  }
  # 6. Calm state.
  list(
    id = "calm",
    severity = "ok",
    title = fpa_txt("synth_find_calm_t"),
    body = fpa_txt("synth_find_calm_b", eur(L$rev_y), eur(L$oi_y), eur(L$cash_year_end)),
    action_label = fpa_txt("synth_find_calm_a"),
    action_tab = "Compte de résultat", action_open = "", action_line = "REV"
  )
}

#' Cash bridge rows from opening to year-end close.
fpa_synth_cash_bridge_rows <- function(cash, fy_months = character()) {
  if (is.null(cash) || !nrow(cash)) return(data.frame())
  pers <- as.character(cash$periode)
  if (length(fy_months)) {
    cash <- cash[pers %in% fy_months, , drop = FALSE]
    if (!nrow(cash)) return(data.frame())
  }
  num <- function(col) {
    if (!col %in% names(cash)) return(0)
    sum(suppressWarnings(as.numeric(cash[[col]])), na.rm = TRUE)
  }
  open0 <- suppressWarnings(as.numeric(cash$cash_open[[1]]))
  close_n <- suppressWarnings(as.numeric(cash$cash_close[[nrow(cash)]]))
  rows <- data.frame(
    item = c(
      "Ouverture", "Résultat net", "Dotations", "Effet de BFR",
      fpa_txt("loc_wc_open_release_short"), "Investissements", "Dette nette", fpa_txt("loc_closing")
    ),
    amount = c(
      open0, num("NI"), num("D_A"), num("wc_impact"),
      num("wc_opening_release"), num("capex"), num("debt_net"), close_n
    ),
    stringsAsFactors = FALSE
  )
  rows$amount[!is.finite(rows$amount)] <- 0
  rows
}

#' Declared bank balances from the journal. A modeled start with a movement is not a declared balance.
#' Withdrawn entries stay in the journal but do not feed Synthèse or the cash form.
fpa_synth_declared_marks <- function(entries) {
  if (!length(entries)) return(NULL)
  rows <- lapply(entries, function(e) {
    if (!fpa_journal_is_active(e)) return(NULL)
    if (!identical(as.character(e$source_metric %||% ""), "outlook_4w")) return(NULL)
    sn <- e$snapshot %||% list()
    if (identical(as.character(sn$opening_source %||% ""), "modeled")) return(NULL)
    as_of <- as.character(sn$as_of %||% e$periode %||% "")
    opening <- suppressWarnings(as.numeric(if (!is.null(sn$opening)) sn$opening else e$amount))
    if (!nzchar(as_of) || length(opening) != 1L || !is.finite(opening)) return(NULL)
    data.frame(
      as_of = as_of, amount = opening, note = as.character(e$note %||% ""),
      stringsAsFactors = FALSE
    )
  })
  rows <- Filter(Negate(is.null), rows)
  if (!length(rows)) return(NULL)
  out <- do.call(rbind, rows)
  out[!duplicated(paste(out$as_of, round(out$amount))), , drop = FALSE]
}

#' Latest declared diamond from the journal. Used when the live outlook file has no opening.
fpa_synth_latest_declared <- function(entries) {
  marks <- fpa_synth_declared_marks(entries)
  if (is.null(marks) || !nrow(marks)) return(NULL)
  ord <- order(as.character(marks$as_of), decreasing = TRUE)
  marks <- marks[ord, , drop = FALSE]
  list(
    opening = suppressWarnings(as.numeric(marks$amount[[1]])),
    as_of = as.character(marks$as_of[[1]]),
    note = as.character(marks$note[[1]] %||% "")
  )
}

#' Gap is material when it exceeds 1 000 EUR or 10 % of the declared balance.
fpa_synth_gap_thr <- function(opening) {
  max(1000, 0.1 * abs(suppressWarnings(as.numeric(opening %||% 0))))
}

#' Active ledger movements plotted on the modeled cash curve (engine-backed).
fpa_synth_ledger_marks <- function(config, cash) {
  if (is.null(config) || !isTRUE(fpa_manual_applied(config))) return(NULL)
  cur <- tryCatch(fpa_manual_read(config), error = function(e) NULL)
  if (is.null(cur) || !isTRUE(cur$ok) || !nrow(cur$data)) return(NULL)
  df <- fpa_manual_active(cur$data)
  if (!nrow(df)) return(NULL)
  rows <- lapply(seq_len(nrow(df)), function(i) {
    d <- fpa_manual_parse_date(df$date_effet[[i]])
    if (!is.finite(d)) return(NULL)
    bal <- fpa_synth_cash_on_date(cash, d)
    if (!is.finite(bal)) bal <- suppressWarnings(as.numeric(df$montant[[i]]))
    kind <- as.character(df$sens[[i]] %||% "out")
    nat <- as.character(df$nature[[i]] %||% "nouveau")
    nat_lab <- if (identical(nat, "decalage")) "Décalage" else "Nouveau"
    kind_lab <- if (identical(kind, "in")) "Encaissement" else "Paiement"
    note <- paste0(
      nat_lab, " · ", kind_lab, " · ", as.character(df$libelle[[i]] %||% ""),
      " · ", fpa_synthese_eur(df$montant[[i]]), " EUR"
    )
    data.frame(
      as_of = as.character(d), amount = bal, note = note, kind = kind,
      stringsAsFactors = FALSE
    )
  })
  rows <- Filter(Negate(is.null), rows)
  if (!length(rows)) return(NULL)
  do.call(rbind, rows)
}

#' Cash movements from the journal and from the live four-week list.
#' Each point is the balance after that movement.
fpa_synth_movement_marks <- function(entries, live = NULL) {
  rows <- list()
  push <- function(lines, opening, note) {
    if (!length(lines) || length(opening) != 1L || !is.finite(opening)) return()
    bal <- opening
    ord <- order(vapply(lines, function(it) as.character(it$date %||% ""), character(1)))
    for (it in lines[ord]) {
      amt <- suppressWarnings(as.numeric(it$amount))
      if (!is.finite(amt)) next
      signed <- if (identical(as.character(it$kind), "out")) -amt else amt
      bal <- bal + signed
      kind <- if (identical(as.character(it$kind), "in")) "Encaissement" else "Paiement"
      bit <- paste0(kind, " · ", as.character(it$label %||% ""), " · ", fpa_synthese_eur(amt), " EUR")
      if (nzchar(note)) bit <- paste0(bit, " · ", note)
      rows[[length(rows) + 1L]] <<- data.frame(
        as_of = as.character(it$date %||% ""),
        amount = bal,
        note = bit,
        kind = as.character(it$kind %||% "out"),
        stringsAsFactors = FALSE
      )
    }
  }
  if (length(entries)) {
    for (e in entries) {
      if (!fpa_journal_is_active(e)) next
      if (!identical(as.character(e$source_metric %||% ""), "outlook_4w")) next
      sn <- e$snapshot %||% list()
      opening <- suppressWarnings(as.numeric(sn$opening))
      push(sn$lines %||% list(), opening, as.character(e$note %||% ""))
    }
  }
  if (!is.null(live)) push(live$lines %||% list(), live$opening, as.character(live$note %||% ""))
  if (!length(rows)) return(NULL)
  out <- do.call(rbind, rows)
  out <- out[nzchar(out$as_of), , drop = FALSE]
  if (!nrow(out)) return(NULL)
  out[!duplicated(paste(out$as_of, round(out$amount), out$note)), , drop = FALSE]
}
fpa_synth_model_cash_at <- function(cash, as_of) {
  as_of <- fpa_outlook_date(as_of)
  if (!is.finite(as_of) || is.null(cash) || !nrow(cash)) return(NA_real_)
  ym <- format(as_of, "%Y-%m")
  i <- match(ym, as.character(cash$periode))
  if (is.na(i)) return(NA_real_)
  suppressWarnings(as.numeric(cash$cash_close[[i]]))
}

#' Approximate cash on a calendar day. The month's flow is spread evenly from opening to close.
fpa_synth_date_fr <- function(d) {
  d <- as.Date(d)
  if (is.na(d)) return("")
  paste(as.integer(format(d, "%d")), fpa_chain_month_fr(as.integer(format(d, "%m"))), format(d, "%Y"))
}

fpa_synth_cash_on_date <- function(cash, on) {
  on <- as.Date(on)
  if (is.na(on) || is.null(cash) || !nrow(cash) || !"periode" %in% names(cash)) return(NA_real_)
  pers <- as.character(cash$periode)
  close_v <- suppressWarnings(as.numeric(cash$cash_close))
  open_v <- if ("cash_open" %in% names(cash)) suppressWarnings(as.numeric(cash$cash_open)) else rep(NA_real_, length(close_v))
  ym <- format(on, "%Y-%m")
  i <- match(ym, pers)
  if (is.na(i)) {
    ok <- grepl("^[0-9]{4}-[0-9]{2}$", pers)
    if (!any(ok)) return(NA_real_)
    if (ym < min(pers[ok])) return(open_v[[which(ok)[1]]])
    return(tail(close_v[ok], 1))
  }
  open_i <- open_v[[i]]
  close_i <- close_v[[i]]
  if (!is.finite(open_i) && i > 1L) open_i <- close_v[[i - 1L]]
  if (!is.finite(close_i)) return(open_i)
  if (!is.finite(open_i)) return(close_i)
  start <- as.Date(paste0(ym, "-01"))
  end <- seq(start, by = "month", length.out = 2L)[[2]] - 1L
  days <- as.integer(end - start) + 1L
  if (!is.finite(days) || days < 1L) return(close_i)
  elapsed <- max(0L, min(days, as.integer(on - start) + 1L))
  open_i + (close_i - open_i) * (elapsed / days)
}

#' Future cash breaches of the floor, separate from historical ones.
fpa_synth_cash_breaches <- function(cash, status = NULL, floor = 0) {
  floor <- suppressWarnings(as.numeric(floor))[1]
  if (!is.finite(floor)) floor <- 0
  empty <- list(hist_n = 0L, fut_n = 0L, first_fut = "", first_fut_v = NA_real_, low_fut = NA_real_, low_fut_m = "")
  if (is.null(cash) || !nrow(cash)) return(empty)
  pers <- as.character(cash$periode)
  close_v <- suppressWarnings(as.numeric(cash$cash_close))
  etat <- rep("inconnu", length(pers))
  if (!is.null(status) && nrow(status) && all(c("periode", "status") %in% names(status))) {
    m <- match(pers, as.character(status$periode))
    hit <- !is.na(m)
    flag <- as.character(status$status)[m[hit]]
    etat[hit] <- ifelse(flag == "forecast", "prévision",
      ifelse(flag %in% c("actual", "réalisé", "closed"), "réalisé", "inconnu"))
  }
  under <- is.finite(close_v) & close_v < floor
  hist <- under & etat == "réalisé"
  fut <- under & etat == "prévision"
  fut_i <- which(fut)
  list(
    hist_n = as.integer(sum(hist)),
    fut_n = as.integer(sum(fut)),
    first_fut = if (length(fut_i)) pers[[fut_i[[1]]]] else "",
    first_fut_v = if (length(fut_i)) close_v[[fut_i[[1]]]] else NA_real_,
    low_fut = if (length(fut_i)) min(close_v[fut_i]) else NA_real_,
    low_fut_m = if (length(fut_i)) pers[[fut_i[[which.min(close_v[fut_i])]]]] else ""
  )
}

#' Material P&L contributors on open months (top 3 by absolute signed move vs closed average).
fpa_synth_driver_rows <- function(pnl, status = NULL, fy_months = character()) {
  empty <- data.frame(
    line = character(), label = character(), effect = numeric(),
    kind = character(), stringsAsFactors = FALSE
  )
  if (is.null(pnl) || !nrow(pnl)) return(empty)
  frame <- fpa_synthese_frame(pnl, status, NULL)
  if (length(fy_months)) frame <- frame[frame$periode %in% fy_months, , drop = FALSE]
  if (!nrow(frame)) return(empty)
  actual <- frame$etat == "réalisé"
  forecast <- frame$etat == "prévision"
  if (!any(actual) || !any(forecast)) return(empty)
  lines <- intersect(c("REV", "COGS", "PAYROLL", "OPEX_OTHER", "OI"), names(frame))
  out <- list()
  for (ln in lines) {
    a <- mean(suppressWarnings(as.numeric(frame[[ln]][actual])), na.rm = TRUE)
    f <- sum(suppressWarnings(as.numeric(frame[[ln]][forecast])), na.rm = TRUE)
    n_f <- sum(forecast)
    if (!is.finite(a) || !is.finite(f) || n_f < 1L) next
    expected <- a * n_f
    delta <- f - expected
    # Income lines: positive delta helps OI; cost lines: sign flipped for effect on result.
    sign_ni <- tryCatch(fpa_pnl_ni_sign(ln), error = function(e) 1)
    effect <- delta * sign_ni
    if (!is.finite(effect) || abs(effect) < 500) next
    out[[length(out) + 1L]] <- data.frame(
      line = ln,
      label = tryCatch(fpa_line_fr(ln), error = function(e) ln),
      effect = effect,
      kind = fpa_txt("loc_scenario_hypothesis"),
      stringsAsFactors = FALSE
    )
  }
  if (!length(out)) return(empty)
  df <- do.call(rbind, out)
  df <- df[order(-abs(df$effect)), , drop = FALSE]
  top <- head(df, 3L)
  # Residual vs open-month OI move relative to closed average (unexplained remainder).
  oi_a <- mean(suppressWarnings(as.numeric(frame$OI[actual])), na.rm = TRUE)
  oi_f <- sum(suppressWarnings(as.numeric(frame$OI[forecast])), na.rm = TRUE)
  n_f <- sum(forecast)
  if (is.finite(oi_a) && is.finite(oi_f) && n_f > 0L) {
    total_move <- oi_f - oi_a * n_f
    explained <- sum(top$effect)
    resid <- total_move - explained
    if (is.finite(resid) && abs(resid) >= 1) {
      top <- rbind(
        top,
        data.frame(
          line = "RESIDUAL",
          label = fpa_txt("loc_residual_unallocated"),
          effect = resid,
          kind = fpa_txt("loc_gap_unexplained"),
          stringsAsFactors = FALSE
        )
      )
    }
  }
  top
}

#' January working-capital release when it is material against a short DSO.
fpa_synth_wc_open_note <- function(cash, fy_months = character()) {
  if (is.null(cash) || !nrow(cash) || !"wc_opening_release" %in% names(cash)) return(NULL)
  pers <- as.character(cash$periode)
  if (length(fy_months)) {
    cash <- cash[pers %in% fy_months, , drop = FALSE]
    pers <- as.character(cash$periode)
  }
  if (!nrow(cash)) return(NULL)
  i <- which(grepl("-01$", pers))
  if (!length(i)) i <- 1L
  i <- i[[1]]
  rel <- suppressWarnings(as.numeric(cash$wc_opening_release[[i]]))
  if (!is.finite(rel) || abs(rel) < 1000) return(NULL)
  list(
    periode = pers[[i]],
    amount = rel,
    sentence = fpa_txt(
      "synth_wc_open_note",
      fpa_ym_fr(pers[[i]]),
      fpa_synthese_eur(abs(rel))
    )
  )
}

#' Open-month hotel figures from volumes already on the book: RevPAR and payroll %.
fpa_synth_hotel_kpis <- function(pnl, status, volumes = NULL, fy_months = character()) {
  out <- list(revpar = NA_real_, revpar_months = character(), payroll_pct = NA_real_)
  if (is.null(pnl) || !nrow(pnl)) return(out)
  pers <- as.character(pnl$periode)
  if (length(fy_months)) {
    keep <- pers %in% fy_months
    pnl <- pnl[keep, , drop = FALSE]
    pers <- as.character(pnl$periode)
  }
  if (!nrow(pnl)) return(out)
  st <- if (!is.null(status) && nrow(status)) {
    status$status[match(pers, as.character(status$periode))]
  } else {
    rep("actual", length(pers))
  }
  open <- pers[as.character(st) %in% c("forecast", "open", "plan")]
  if (!length(open) && !is.null(volumes) && nrow(volumes)) {
    open <- intersect(pers, as.character(volumes$periode))
  }
  if (length(open) && !is.null(volumes) && nrow(volumes) &&
      all(c("periode", "taux_utilisation", "tarif") %in% names(volumes))) {
    v <- volumes[as.character(volumes$periode) %in% open, , drop = FALSE]
    occ <- suppressWarnings(as.numeric(v$taux_utilisation))
    rate <- suppressWarnings(as.numeric(v$tarif))
    ok <- is.finite(occ) & is.finite(rate)
    if (any(ok)) {
      out$revpar <- mean(occ[ok] * rate[ok], na.rm = TRUE)
      out$revpar_months <- as.character(v$periode[ok])
    }
  }
  if ("PAYROLL" %in% names(pnl) && "REV" %in% names(pnl)) {
    pay <- sum(suppressWarnings(as.numeric(pnl$PAYROLL)), na.rm = TRUE)
    rev <- sum(suppressWarnings(as.numeric(pnl$REV)), na.rm = TRUE)
    if (is.finite(pay) && is.finite(rev) && rev > 1) out$payroll_pct <- pay / rev
  }
  out
}

#' One deterministic presentation object for Synthèse.
fpa_synthese_model <- function(config, finance, outlook_saved = NULL, cash_floor = 0, today = Sys.Date(),
                               journal_entries = NULL, volumes = NULL) {
  pnl <- finance$pnl
  cash <- finance$cash
  status <- finance$period_status
  budget <- finance$budget
  if (is.null(volumes)) volumes <- finance$volumes
  pers <- if (!is.null(pnl) && nrow(pnl)) as.character(pnl$periode) else character()
  ye <- tryCatch(fpa_year_end_info(config %||% list(), status, pers), error = function(e) {
    list(close = "", measured = "", months = character(), fy_start = "", beyond = FALSE)
  })
  fy_months <- as.character(ye$months %||% character())
  if (!length(fy_months) && nzchar(ye$close %||% "")) fy_months <- fpa_fy_months(ye$close)
  frame <- fpa_synthese_frame(pnl, status, NULL)
  picks <- tryCatch(fpa_cash_period_picks(cash, status), error = function(e) NULL)
  L <- fpa_synthese_land(
    frame, budget,
    cash_close = if (!is.null(picks)) picks$closed else NA_real_,
    cash_year_end = if (!is.null(picks)) picks$year_end else NA_real_,
    cash_year_end_month = if (!is.null(picks)) picks$year_end_month else ye$measured,
    cash_low = if (!is.null(picks)) picks$low else NA_real_,
    cash_low_month = if (!is.null(picks)) picks$low_month else "",
    cash_under = if (!is.null(picks)) picks$under else 0L,
    fy_months = fy_months
  )
  # Prefer year-end metric on fiscal close when available.
  if (isTRUE(L$ok) && nzchar(ye$close %||% "") && !is.null(cash) && nrow(cash)) {
    ye_cash <- tryCatch(fpa_objectif_read(list(cash = cash), "cash_ye", ye$close), error = function(e) NA_real_)
    if (is.finite(ye_cash)) {
      L$cash_year_end <- ye_cash
      L$cash_year_end_month <- ye$measured %||% ye$close
      if (is.finite(L$cash_close)) L$cash_delta <- L$cash_year_end - L$cash_close
    }
  }
  roll <- if (is.null(outlook_saved)) NULL else {
    tryCatch(fpa_outlook_roll(outlook_saved$opening, outlook_saved$items, outlook_saved$as_of, FALSE), error = function(e) NULL)
  }
  outlook <- fpa_synth_outlook_state(outlook_saved, roll, today)
  # A journal diamond is a declared balance even when cash-outlook.json has no opening.
  if (identical(outlook$state, "need_balance") || !is.finite(outlook$opening)) {
    declared <- fpa_synth_latest_declared(journal_entries)
    if (!is.null(declared) && is.finite(declared$opening)) {
      outlook$opening <- declared$opening
      outlook$as_of <- declared$as_of
      if (identical(outlook$state, "need_balance")) {
        outlook$state <- if ((outlook$n_items %||% 0L) < 1L) "balance_only" else outlook$state
        if (identical(outlook$state, "need_balance")) outlook$state <- "balance_only"
      }
      outlook$from_journal <- TRUE
      as_of_d <- fpa_outlook_date(outlook$as_of)
      outlook$planned <- is.finite(as_of_d) && is.finite(as.Date(today)) && as_of_d > as.Date(today)
      if (isTRUE(outlook$planned)) outlook$stale <- FALSE
    }
  }
  model_at <- if (nzchar(outlook$as_of %||% "")) fpa_synth_model_cash_at(cash, outlook$as_of) else NA_real_
  cash_today <- fpa_synth_cash_on_date(cash, today)
  trust <- fpa_synth_trust(L, outlook, cash_floor, model_at)
  finding <- if (isTRUE(L$ok)) fpa_synth_finding(L, trust, outlook, cash_floor, model_at, cash_today, today) else {
    list(
      id = "empty", severity = "watch", title = fpa_txt("synth_find_empty_t"),
      body = fpa_txt("synth_find_empty_b"), action_label = fpa_txt("synth_find_empty_a"),
      action_tab = "Paramètres", action_open = "", action_line = ""
    )
  }
  breaches <- fpa_synth_cash_breaches(cash, status, cash_floor)
  bridge <- fpa_synth_cash_bridge_rows(cash, fy_months)
  drivers <- fpa_synth_driver_rows(pnl, status, fy_months)
  hotel <- fpa_synth_hotel_kpis(pnl, status, volumes, fy_months)
  wc_note <- fpa_synth_wc_open_note(cash, fy_months)
  company <- as.character((config$company %||% list())$name %||% "")
  currency <- as.character((config$company %||% list())$currency %||% "EUR")
  list(
    ok = isTRUE(L$ok),
    context = list(
      company = company,
      currency = currency,
      fy_close = ye$close %||% "",
      fy_start = ye$fy_start %||% "",
      fy_months = fy_months,
      closed_through = if (!is.null(picks)) picks$closed_month else "",
      measured = ye$measured %||% "",
      beyond = isTRUE(ye$beyond)
    ),
    land = L,
    outlook = outlook,
    trust = trust,
    finding = finding,
    breaches = breaches,
    bridge = bridge,
    drivers = drivers,
    hotel = hotel,
    wc_open = wc_note,
    model_cash_at_asof = model_at,
    cash_today = cash_today,
    cash_floor = suppressWarnings(as.numeric(cash_floor))[1],
    frame = frame,
    picks = picks
  )
}

#' Functions that fpa_run() needs. A missing name is a load contract failure.
fpa_run_required_funs <- function() {
  c(
    "fpa_load_config", "fpa_paths", "fpa_read_all_financial", "fpa_validate",
    "fpa_read_assumptions", "fpa_read_decisions", "fpa_read_volumes",
    "fpa_budget_monthly", "fpa_build_pnl", "fpa_decisions_dated",
    "fpa_decisions_ledger", "fpa_da_from_capex", "fpa_wc_opening_release",
    "fpa_wc_opening_flags", "fpa_build_cash_bridge", "fpa_build_bs",
    "fpa_period_status", "fpa_variance_build"
  )
}

fpa_run_contract_ok <- function() {
  need <- fpa_run_required_funs()
  miss <- need[!vapply(need, exists, logical(1), mode = "function")]
  list(ok = !length(miss), missing = miss)
}
