# SPDX-License-Identifier: AGPL-3.0-or-later
# Copyright (C) 2026 Julio Bauer Cantuaria
# fpa-health.R — Ratios the books can support.
# LAST CHANGE: 2026-09-29
# A hotel with cash, a long loan, and almost no stock is not a sugar mill.
# Cost per tonne and a buyer multiple are absent. What remains is read off
# the balance sheet and the year-to-date statement. The closed month is the
# snapshot. A later month is a path, computed by the caller.

fpa_health_div <- function(a, b, floor = 0.5) {
  a <- suppressWarnings(as.numeric(a))
  b <- suppressWarnings(as.numeric(b))
  if (!is.finite(a) || !is.finite(b) || abs(b) < floor) return(NA_real_)
  a / b
}

fpa_health_num <- function(df, per, col) {
  if (is.null(df) || !nrow(df) || !col %in% names(df)) return(NA_real_)
  i <- match(as.character(per), as.character(df$periode))
  if (is.na(i)) return(NA_real_)
  suppressWarnings(as.numeric(df[[col]][[i]]))
}

fpa_health_sum <- function(df, pers, col) {
  if (is.null(df) || !nrow(df) || !col %in% names(df) || !length(pers)) return(NA_real_)
  hit <- as.character(df$periode) %in% as.character(pers)
  if (!any(hit)) return(NA_real_)
  sum(suppressWarnings(as.numeric(df[[col]][hit])), na.rm = TRUE)
}

fpa_health_days <- function(per) {
  start <- as.Date(paste0(as.character(per), "-01"))
  if (is.na(start)) return(NA_real_)
  as.numeric(seq(start, by = "month", length.out = 2L)[[2]] - start)
}

#' Last month whose status is not a forecast.
fpa_health_closed_month <- function(status) {
  if (is.null(status) || !nrow(status) || !all(c("periode", "status") %in% names(status))) {
    return("")
  }
  act <- as.character(status$periode[as.character(status$status) != "forecast"])
  if (!length(act)) return("")
  act[[length(act)]]
}

fpa_health_ids <- function() {
  c(
    "current_ratio", "quick_ratio", "cash_ratio",
    "debt_to_equity", "debt_to_assets", "interest_coverage",
    "dso_days", "dio_days", "asset_turnover",
    "gross_margin", "ebitda_margin", "oi_margin", "roa",
    "book_ev", "roce"
  )
}

#' Popup parts for one ratio. `files` is bs, pnl, or both.
#' Each part is op, copy key, and a, b, c, or days.
fpa_health_explain <- function(id) {
  part <- function(op, key, field) list(op = op, key = key, field = field)
  spec <- list(
    current_ratio = list(files = "bs", parts = list(
      part("", "health_lab_ca", "a"), part("/", "health_lab_cl", "b")
    )),
    quick_ratio = list(files = "bs", parts = list(
      part("", "health_lab_quick", "a"), part("/", "health_lab_cl", "b")
    )),
    cash_ratio = list(files = "bs", parts = list(
      part("", "health_lab_cash", "a"), part("/", "health_lab_cl", "b")
    )),
    debt_to_equity = list(files = "bs", parts = list(
      part("", "health_lab_debt", "a"), part("/", "health_lab_eq", "b")
    )),
    debt_to_assets = list(files = "bs", parts = list(
      part("", "health_lab_debt", "a"), part("/", "health_lab_assets", "b")
    )),
    interest_coverage = list(files = "pnl", parts = list(
      part("", "health_lab_oi", "a"), part("/", "health_lab_fin", "b")
    )),
    dso_days = list(files = "both", parts = list(
      part("", "health_lab_ar", "a"), part("\u00d7", "health_lab_days", "days"), part("/", "health_lab_rev", "b")
    )),
    dio_days = list(files = "both", parts = list(
      part("", "health_lab_inv", "a"), part("\u00d7", "health_lab_days", "days"), part("/", "health_lab_cogs", "b")
    )),
    asset_turnover = list(files = "both", parts = list(
      part("", "health_lab_rev_y", "a"), part("/", "health_lab_assets", "b")
    )),
    gross_margin = list(files = "pnl", parts = list(
      part("", "health_lab_gp", "a"), part("/", "health_lab_rev_y", "b")
    )),
    ebitda_margin = list(files = "pnl", parts = list(
      part("", "health_lab_ebitda", "a"), part("/", "health_lab_rev_y", "b")
    )),
    oi_margin = list(files = "pnl", parts = list(
      part("", "health_lab_oi", "a"), part("/", "health_lab_rev_y", "b")
    )),
    roa = list(files = "both", parts = list(
      part("", "health_lab_oi", "a"), part("/", "health_lab_assets", "b")
    )),
    book_ev = list(files = "bs", parts = list(
      part("", "health_lab_debt", "a"), part("+", "health_lab_eq", "b"), part("\u2212", "health_lab_cash", "c")
    )),
    roce = list(files = "both", parts = list(
      part("", "health_lab_oi", "a"), part("/", "health_lab_book", "b")
    ))
  )
  spec[[as.character(id)]]
}

#' Copy keys: definition, then why it matters.
fpa_health_copy_keys <- function(id) {
  switch(as.character(id),
    current_ratio = c("health_def_current", "health_why_current"),
    quick_ratio = c("health_def_quick", "health_why_quick"),
    cash_ratio = c("health_def_cash", "health_why_cash"),
    debt_to_equity = c("health_def_de", "health_why_de"),
    debt_to_assets = c("health_def_da", "health_why_da"),
    interest_coverage = c("health_def_cover", "health_why_cover"),
    dso_days = c("health_def_dso", "health_why_dso"),
    dio_days = c("health_def_dio", "health_why_dio"),
    asset_turnover = c("health_def_turn", "health_why_turn"),
    gross_margin = c("health_def_gm", "health_why_gm"),
    ebitda_margin = c("health_def_ebitda", "health_why_ebitda"),
    oi_margin = c("health_def_om", "health_why_om"),
    roa = c("health_def_roa", "health_why_roa"),
    book_ev = c("health_def_ev", "health_why_ev"),
    roce = c("health_def_roce", "health_why_roce"),
    character()
  )
}

#' Fiscal year-end month from company settings. December when unset.
fpa_fy_end_month <- function(config) {
  fy <- NA_integer_
  co <- if (is.list(config)) config$company else NULL
  if (is.list(co) && !is.null(co$fiscal_year_end_month)) {
    fy <- suppressWarnings(as.integer(co$fiscal_year_end_month[[1]]))
  }
  if (!is.finite(fy) && is.list(config) && !is.null(config$fiscal_year_end_month)) {
    fy <- suppressWarnings(as.integer(config$fiscal_year_end_month[[1]]))
  }
  if (!is.finite(fy) && is.list(config) && !is.null(config$fy_end_month)) {
    fy <- suppressWarnings(as.integer(config$fy_end_month[[1]]))
  }
  if (!is.finite(fy) || fy < 1L || fy > 12L) fy <- 12L
  as.integer(fy)
}

#' First month of the fiscal year. A December close starts in January.
fpa_fy_start_month <- function(config) {
  as.integer((fpa_fy_end_month(config) %% 12L) + 1L)
}

#' Periods from the fiscal-year start through `per`, including a year that crosses January.
fpa_health_ytd_pers <- function(periods, per, fy_start = 1L) {
  per <- as.character(per)
  periods <- as.character(periods)
  fy_start <- suppressWarnings(as.integer(if (is.null(fy_start)) 1L else fy_start[[1]]))
  if (!length(fy_start) || !is.finite(fy_start[[1]]) || fy_start[[1]] < 1L || fy_start[[1]] > 12L) fy_start <- 1L
  fy_start <- as.integer(fy_start[[1]])
  y <- suppressWarnings(as.integer(substr(per, 1L, 4L)))
  m <- suppressWarnings(as.integer(substr(per, 6L, 7L)))
  if (!is.finite(y) || !is.finite(m)) return(character())
  start_year <- if (m >= fy_start) y else y - 1L
  start <- sprintf("%04d-%02d", start_year, fy_start)
  periods[periods >= start & periods <= per]
}

#' One month. Flows run from the first month of the fiscal year through the month.
#' `fy_start` is 1 for a December close. `a` and `b` (and `c` for the book value) are the amounts behind the ratio.
fpa_health_month <- function(bs, pnl, per, fy_start = 1L) {
  empty <- data.frame(
    id = character(), value = numeric(), a = numeric(), b = numeric(), c = numeric(),
    stringsAsFactors = FALSE
  )
  per <- as.character(per)
  if (!nzchar(per) || is.null(bs) || !nrow(bs) || is.null(pnl) || !nrow(pnl)) return(empty)
  if (!per %in% as.character(bs$periode)) return(empty)
  ytd <- fpa_health_ytd_pers(pnl$periode, per, fy_start)

  cash <- fpa_health_num(bs, per, "CASH")
  ar <- fpa_health_num(bs, per, "AR")
  inv <- fpa_health_num(bs, per, "INV")
  oca <- fpa_health_num(bs, per, "OTHER_CA")
  ap <- fpa_health_num(bs, per, "AP")
  st <- fpa_health_num(bs, per, "ST_DEBT")
  lt <- fpa_health_num(bs, per, "LT_DEBT")
  ocl <- fpa_health_num(bs, per, "OTHER_CL")
  ppe <- fpa_health_num(bs, per, "PPE")
  eq <- fpa_health_num(bs, per, "EQUITY")
  z <- function(x) if (is.finite(x)) x else 0
  ca <- z(cash) + z(ar) + z(inv) + z(oca)
  cl <- z(ap) + z(st) + z(ocl)
  assets <- ca + z(ppe)
  debt <- z(st) + z(lt)
  rev_m <- fpa_health_num(pnl, per, "REV")
  cogs_m <- fpa_health_num(pnl, per, "COGS")
  rev_y <- fpa_health_sum(pnl, ytd, "REV")
  gp_y <- fpa_health_sum(pnl, ytd, "GP")
  ebitda_y <- fpa_health_sum(pnl, ytd, "EBITDA")
  oi_y <- fpa_health_sum(pnl, ytd, "OI")
  fin_y <- fpa_health_sum(pnl, ytd, "FIN")
  days <- fpa_health_days(per)
  book <- debt + z(eq) - z(cash)

  row <- function(id, value, a, b, c = NA_real_) {
    data.frame(id = id, value = value, a = a, b = b, c = c, stringsAsFactors = FALSE)
  }
  rbind(
    row("current_ratio", fpa_health_div(ca, cl), ca, cl),
    row("quick_ratio", fpa_health_div(z(cash) + z(ar), cl), z(cash) + z(ar), cl),
    row("cash_ratio", fpa_health_div(cash, cl), z(cash), cl),
    row("debt_to_equity", fpa_health_div(debt, eq), debt, z(eq)),
    row("debt_to_assets", fpa_health_div(debt, assets), debt, assets),
    row("interest_coverage", fpa_health_div(oi_y, fin_y), oi_y, fin_y),
    row("dso_days", fpa_health_div(ar, rev_m, 1) * days, z(ar), z(rev_m)),
    row("dio_days", fpa_health_div(inv, cogs_m, 1) * days, z(inv), z(cogs_m)),
    row("asset_turnover", fpa_health_div(rev_y, assets), rev_y, assets),
    row("gross_margin", fpa_health_div(gp_y, rev_y, 1), gp_y, rev_y),
    row("ebitda_margin", fpa_health_div(ebitda_y, rev_y, 1), ebitda_y, rev_y),
    row("oi_margin", fpa_health_div(oi_y, rev_y, 1), oi_y, rev_y),
    row("roa", fpa_health_div(oi_y, assets, 1), oi_y, assets),
    row("book_ev", book, debt, z(eq), z(cash)),
    row("roce", fpa_health_div(oi_y, book, 1), oi_y, book)
  )
}

#' One ratio, in the same units as the Health cards.
fpa_health_fmt <- function(id, value) {
  value <- suppressWarnings(as.numeric(value))
  if (!length(value) || !is.finite(value[[1]])) return("\u2014")
  value <- value[[1]]
  en <- exists("fpa_lang_current", mode = "function") && identical(fpa_lang_current(), "en")
  mark <- if (en) "." else ","
  if (id %in% c("gross_margin", "ebitda_margin", "oi_margin", "roa", "roce")) {
    return(paste0(formatC(100 * value, format = "f", digits = 1, decimal.mark = mark), " %"))
  }
  if (id %in% c("dso_days", "dio_days")) {
    return(paste0(formatC(value, format = "f", digits = 1, decimal.mark = mark), if (en) " d" else " j"))
  }
  if (identical(id, "book_ev")) {
    return(paste0(format(round(value), big.mark = " ", scientific = FALSE, trim = TRUE), " EUR"))
  }
  digits <- if (abs(value) >= 100) 0 else 2
  paste0(formatC(value, format = "f", digits = digits, decimal.mark = mark), " x")
}

# Usual planning bands used by credit and FP&A practice.
# dir up_soft: room to spare is not a prize. down: lower is lighter. up: higher is stronger.
# asset_turnover and book_ev have no single band.
FPA_HEALTH_GAUGE <- list(
  current_ratio = list(lo = 1, hi = 2, scale = 3, dir = "up_soft"),
  quick_ratio = list(lo = 1, hi = 1.5, scale = 2.5, dir = "up_soft"),
  cash_ratio = list(lo = 0.2, hi = 0.5, scale = 1.5, dir = "up_soft"),
  debt_to_equity = list(lo = 1, hi = 2, scale = 3, dir = "down"),
  debt_to_assets = list(lo = 0.4, hi = 0.6, scale = 1, dir = "down"),
  interest_coverage = list(lo = 1.5, hi = 3, scale = 8, dir = "up"),
  dso_days = list(lo = 15, hi = 45, scale = 60, dir = "down"),
  dio_days = list(lo = 15, hi = 45, scale = 60, dir = "down"),
  gross_margin = list(lo = 0.5, hi = 0.8, scale = 1, dir = "up"),
  ebitda_margin = list(lo = 0.1, hi = 0.2, scale = 0.45, dir = "up"),
  oi_margin = list(lo = 0.1, hi = 0.2, scale = 0.45, dir = "up"),
  roa = list(lo = 0.05, hi = 0.15, scale = 0.35, dir = "up"),
  roce = list(lo = 0.05, hi = 0.15, scale = 0.35, dir = "up")
)

fpa_health_gauge_spec <- function(id) {
  FPA_HEALTH_GAUGE[[id]]
}

fpa_health_gauge_zone <- function(id, value) {
  s <- fpa_health_gauge_spec(id)
  if (is.null(s) || !is.finite(value)) return(NULL)
  zone <- if (value < s$lo) "low" else if (value <= s$hi) "mid" else "high"
  key <- if (identical(s$dir, "down") && id %in% c("dso_days", "dio_days")) {
    switch(zone, low = "health_band_short", mid = "health_band_ordinary", "health_band_slow")
  } else if (identical(s$dir, "down")) {
    switch(zone, low = "health_band_light", mid = "health_band_ordinary", "health_band_heavy")
  } else if (identical(s$dir, "up")) {
    switch(zone, low = "health_band_thin", mid = "health_band_ordinary", "health_band_strong")
  } else {
    switch(zone, low = "health_band_tight", mid = "health_band_ordinary", "health_band_ample")
  }
  list(
    spec = s, key = key, beyond = isTRUE(value > s$scale),
    pos = min(1, max(0, value / s$scale)),
    dir = s$dir, zone = zone
  )
}

#' 0 at the weak end of that ratio's gauge, 1 at the strong end.
#' A ratio with no usual band (asset turnover, book value) stays out.
fpa_health_strength <- function(id, value) {
  g <- fpa_health_gauge_zone(id, value)
  if (is.null(g)) return(NA_real_)
  if (identical(g$dir, "down")) 1 - g$pos else g$pos
}

#' One mark for a folded card: the mean of the gauges that have a band.
#' `skip` ids are left out of the mean (a ratio over a denominator too small to read).
fpa_health_fold_mix <- function(ids, values, skip = character()) {
  ids <- as.character(ids)
  values <- suppressWarnings(as.numeric(values))
  if (length(values) != length(ids)) values <- rep(NA_real_, length(ids))
  scores <- vapply(seq_along(ids), function(i) fpa_health_strength(ids[[i]], values[[i]]), numeric(1))
  scores[ids %in% as.character(skip)] <- NA_real_
  used <- is.finite(scores)
  list(
    pos = if (any(used)) mean(scores[used]) else NA_real_,
    n_used = sum(used),
    n_all = length(ids),
    skipped = intersect(ids, as.character(skip))
  )
}

#' Liquidity ratios that divide by current liabilities too small to read.
#' Same rule as the anomaly flag: cash of at least 10 000 EUR, current
#' liabilities under 1 000 EUR, and cash at least ten times those liabilities.
fpa_health_thin_ids <- function(month_rows) {
  if (is.null(month_rows) || !nrow(month_rows) || !"cash_ratio" %in% month_rows$id) return(character())
  i <- match("cash_ratio", month_rows$id)
  cash <- suppressWarnings(as.numeric(month_rows$a[[i]]))
  cl <- suppressWarnings(as.numeric(month_rows$b[[i]]))
  if (!is.finite(cash) || !is.finite(cl)) return(character())
  if (cl >= 1000 || cash < 10000 || cash < 10 * cl) return(character())
  intersect(c("current_ratio", "quick_ratio", "cash_ratio"), month_rows$id)
}

#' Third of the folded gauge the mark sits in: weak, middle, or strong.
fpa_health_fold_zone <- function(pos) {
  pos <- suppressWarnings(as.numeric(pos[[1]]))
  if (!is.finite(pos)) return("")
  if (pos < 1 / 3) "attn" else if (pos < 2 / 3) "mid" else "good"
}

#' French day label, without relying on the Windows locale. 7 oct. 2026.
fpa_header_fr_date <- function(d) {
  mois <- c(
    fpa_txt("month_s_1"), fpa_txt("month_s_2"), fpa_txt("month_s_3"), fpa_txt("month_s_4"), fpa_txt("month_s_5"), fpa_txt("month_s_6"),
    fpa_txt("month_s_7"), fpa_txt("month_s_8"), fpa_txt("month_s_9"), fpa_txt("month_s_10"), fpa_txt("month_s_11"), fpa_txt("month_s_12")
  )
  d <- as.Date(d)
  sprintf(
    "%d %s %d",
    as.integer(format(d, "%d")),
    mois[[as.integer(format(d, "%m"))]],
    as.integer(format(d, "%Y"))
  )
}

#' Where today sits in the fiscal year, and how many days remain in this month.
#' `pct` counts today. A December close runs 1 January to 31 December.
fpa_header_clock <- function(config, today = Sys.Date()) {
  today <- as.Date(today)
  if (length(today) != 1L || is.na(today)) today <- Sys.Date()
  fy_end <- fpa_fy_end_month(config)
  fy_start <- fpa_fy_start_month(config)
  y <- as.integer(format(today, "%Y"))
  m <- as.integer(format(today, "%m"))
  start_year <- if (m >= fy_start) y else y - 1L
  end_year <- if (fy_end >= fy_start) start_year else start_year + 1L
  start <- as.Date(sprintf("%04d-%02d-01", start_year, fy_start))
  end_first <- as.Date(sprintf("%04d-%02d-01", end_year, fy_end))
  end <- seq(end_first, by = "month", length.out = 2L)[[2]] - 1
  span <- as.integer(end - start) + 1L
  done <- as.integer(today - start) + 1L
  if (!is.finite(done) || done < 0L) done <- 0L
  if (done > span) done <- span
  pct <- if (span > 0L) as.integer(round(100 * done / span)) else 0L
  month_first <- as.Date(sprintf("%04d-%02d-01", y, m))
  month_end <- seq(month_first, by = "month", length.out = 2L)[[2]] - 1
  left <- as.integer(month_end - today)
  if (!is.finite(left) || left < 0L) left <- 0L
  list(
    today = today, pct = pct, left = left,
    start = start, end = end, month_end = month_end,
    span = span, done = done
  )
}

#' Map a lead ratio to Anomalies-style bands: act / watch / hold.
fpa_health_fold_band <- function(id, value) {
  g <- fpa_health_gauge_zone(id, value)
  if (is.null(g)) return("hold")
  zone <- g$zone
  if (identical(g$dir, "down")) {
    switch(zone, low = "hold", mid = "watch", "act")
  } else if (identical(g$dir, "up")) {
    switch(zone, low = "act", mid = "watch", "hold")
  } else {
    switch(zone, low = "act", mid = "hold", "watch")
  }
}
