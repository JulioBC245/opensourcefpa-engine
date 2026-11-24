# SPDX-License-Identifier: AGPL-3.0-or-later
# Copyright (C) 2026 Julio Bauer Cantuaria
# fpa-scenario.R — Named scenarios from levers moved in days or in percent.
# LAST CHANGE: 2026-09-22
# A lever is a move away from today's value, not a typed absolute. Days stay days.
# Nothing here writes a file. A scenario lives in the session, and the actuals on
# disk are never touched.

#' One assumption_keys row, matched on id. NULL when the config has no row.
fpa_scenario_find <- function(keys, id) {
  if (is.null(keys)) return(NULL)
  for (k in keys) {
    if (identical(as.character(k$id %||% ""), as.character(id))) return(k)
  }
  NULL
}

fpa_scenario_num <- function(x, fallback) {
  v <- suppressWarnings(as.numeric(x))
  if (length(v) != 1L || !is.finite(v)) fallback else v
}

#' Slider shape. The assumption_keys row wins when it declares lever.
#' The id guess is only for a config written before that field existed.
fpa_scenario_lever_spec <- function(id, key = NULL) {
  declared <- tolower(as.character(key$lever %||% ""))
  if (length(declared) == 1L && declared %in% c("days", "points", "percent")) {
    defaults <- switch(
      declared,
      days = list(min = -21, max = 21, step = 7),
      points = list(min = -5, max = 5, step = 1),
      list(min = -30, max = 30, step = 5)
    )
    unit <- switch(declared, days = " j", points = " pts", " %")
    return(list(
      kind = declared,
      unit = unit,
      min = fpa_scenario_num(key$min, defaults$min),
      max = fpa_scenario_num(key$max, defaults$max),
      step = fpa_scenario_num(key$step, defaults$step)
    ))
  }
  guess <- tolower(as.character(id %||% ""))
  if (grepl("dso|dpo|dio|delai|jour", guess)) {
    return(list(kind = "days", unit = " j", min = -21, max = 21, step = 7))
  }
  if (grepl("taux|impot|rate", guess)) {
    return(list(kind = "points", unit = " pts", min = -5, max = 5, step = 1))
  }
  list(kind = "percent", unit = " %", min = -30, max = 30, step = 5)
}

#' Slider title. label on the assumption_keys row, then a short meaning, then the id.
fpa_scenario_label <- function(id, key = NULL) {
  lab <- as.character(key$label %||% "")
  if (length(lab) == 1L && !is.na(lab) && nzchar(lab)) return(lab)
  meaning <- as.character(key$meaning %||% "")
  if (length(meaning) == 1L && !is.na(meaning) && nzchar(meaning) && nchar(meaning) <= 40L) {
    return(meaning)
  }
  guess <- tolower(as.character(id %||% ""))
  if (identical(guess, "dso")) return("Encaissement clients")
  if (identical(guess, "dpo")) return("Paiement fournisseurs")
  if (identical(guess, "dio")) return("Rotation des stocks")
  if (grepl("impot", guess)) return("Taux d'impôt")
  if (grepl("loyer", guess)) return("Loyer")
  if (grepl("charge\\.fixe", guess)) return(paste("Charge fixe", sub("^charge\\.fixe\\.", "", guess)))
  as.character(id)
}

#' P&L lines a scenario may scale, and the slider range on each.
#' Absent scenario_lines keeps the four lines that older configs implied.
#' An empty array means the config asked for none.
fpa_scenario_lines <- function(config, pnl_names) {
  declared <- config$scenario_lines
  names_have <- as.character(pnl_names %||% character(0))
  if (is.null(declared)) {
    ids <- intersect(c("REV", "COGS", "PAYROLL", "OPEX_OTHER"), names_have)
    spec <- lapply(ids, function(id) list(min = -20, max = 20, step = 5))
    names(spec) <- ids
    return(list(ids = ids, spec = spec))
  }
  ids <- character(0)
  spec <- list()
  for (row in declared) {
    if (is.null(row$id)) {
      id <- as.character(row)[[1]]
      range <- list(min = -20, max = 20, step = 5)
    } else {
      id <- as.character(row$id)
      range <- list(
        min = fpa_scenario_num(row$min, -20),
        max = fpa_scenario_num(row$max, 20),
        step = fpa_scenario_num(row$step, 5)
      )
    }
    if (!nzchar(id) || !(id %in% names_have) || id %in% ids) next
    ids <- c(ids, id)
    spec[[id]] <- range
  }
  list(ids = ids, spec = spec)
}

#' Absolute value the engine needs, from a move the user made.
#' A rate held as a fraction takes points as hundredths.
fpa_scenario_value <- function(spec, base, delta) {
  base <- suppressWarnings(as.numeric(base))
  delta <- suppressWarnings(as.numeric(delta))
  if (!is.finite(base) || !is.finite(delta)) return(NA_real_)
  if (identical(spec$kind, "days")) return(base + delta)
  if (identical(spec$kind, "points")) {
    return(if (abs(base) <= 1) base + delta / 100 else base + delta)
  }
  base * (1 + delta / 100)
}

#' Latest value of each assumption, which is what a lever moves away from.
fpa_scenario_base_values <- function(wide, ids) {
  out <- list()
  if (is.null(wide) || !nrow(wide)) return(out)
  for (id in ids) {
    if (!(id %in% names(wide))) next
    v <- suppressWarnings(as.numeric(wide[[id]][nrow(wide)]))
    if (is.finite(v)) out[[id]] <- v
  }
  out
}

#' Named list of absolute assumption values, from non-zero moves only.
#' keys is config$assumption_keys. A declared lever overrides the id guess.
fpa_scenario_overrides <- function(base_values, deltas, keys = NULL) {
  out <- list()
  for (id in names(deltas)) {
    d <- suppressWarnings(as.numeric(deltas[[id]]))
    if (!is.finite(d) || d == 0) next
    base <- base_values[[id]]
    if (is.null(base)) next
    v <- fpa_scenario_value(fpa_scenario_lever_spec(id, fpa_scenario_find(keys, id)), base, d)
    if (is.finite(v)) out[[id]] <- v
  }
  out
}

#' Multiplier per P&L line, from a percent move. Forecast months only, which
#' fpa_apply_line_scale already enforces.
fpa_scenario_line_scale <- function(line_deltas) {
  out <- list()
  for (nm in names(line_deltas)) {
    d <- suppressWarnings(as.numeric(line_deltas[[nm]]))
    if (!is.finite(d) || d == 0) next
    out[[nm]] <- 1 + d / 100
  }
  out
}

#' One sentence per moved lever, in the unit the user moved.
fpa_scenario_moves <- function(base_values, deltas, line_deltas, keys = NULL) {
  bits <- character(0)
  for (id in names(deltas)) {
    d <- suppressWarnings(as.numeric(deltas[[id]]))
    if (!is.finite(d) || d == 0) next
    row <- fpa_scenario_find(keys, id)
    spec <- fpa_scenario_lever_spec(id, row)
    unit <- if (identical(spec$kind, "days")) "jours" else if (identical(spec$kind, "points")) "points" else "%"
    bits <- c(bits, paste0(fpa_scenario_label(id, row), " ", if (d > 0) "+" else "", d, " ", unit))
  }
  for (nm in names(line_deltas)) {
    d <- suppressWarnings(as.numeric(line_deltas[[nm]]))
    if (!is.finite(d) || d == 0) next
    bits <- c(bits, paste0(nm, " ", if (d > 0) "+" else "", d, " %"))
  }
  bits
}

#' Run the engine with the levers applied. Same return shape as fpa_run.
fpa_scenario_run <- function(config_path, base_values, deltas = list(), line_deltas = list(), keys = NULL) {
  ov <- fpa_scenario_overrides(base_values, deltas, keys)
  ls <- fpa_scenario_line_scale(line_deltas)
  fpa_run(
    config_path, write = FALSE, stop_on_error = FALSE,
    assumption_override = if (length(ov)) ov else NULL,
    line_scale = if (length(ls)) ls else NULL
  )
}

#' Cash read of one run: opening, closing, lowest month, months under a floor.
#' oi_fy is operating income over the fiscal year that ends on ye_month (all rows
#' when ye_month is empty); oi_last is operating income of that close month.
#' oi stays as an alias of oi_fy for older callers.
fpa_scenario_cash_kpi <- function(res, floor = 0, ye_month = "") {
  empty <- list(
    open = NA_real_, close = NA_real_, low = NA_real_, low_month = "", under = 0L,
    oi = NA_real_, oi_fy = NA_real_, oi_last = NA_real_, ye_month = ""
  )
  if (is.null(res) || is.null(res$cash) || !nrow(res$cash)) return(empty)
  cash <- res$cash
  close_v <- suppressWarnings(as.numeric(cash$cash_close))
  if (!any(is.finite(close_v))) return(empty)
  i <- which.min(close_v)
  fl <- suppressWarnings(as.numeric(floor))
  if (!is.finite(fl)) fl <- 0
  ye_month <- as.character(ye_month %||% "")
  oi_v <- suppressWarnings(as.numeric(res$pnl$OI %||% numeric(0)))
  has_per <- !is.null(res$pnl) && "periode" %in% names(res$pnl)
  if (nzchar(ye_month) && has_per) {
    oi_fy <- fpa_objectif_read(res, "oi_fy", ye_month)
    oi_last <- fpa_objectif_read(res, "oi_ye", ye_month)
    close <- fpa_objectif_read(res, "cash_ye", ye_month)
  } else {
    oi_fy <- sum(oi_v, na.rm = TRUE)
    oi_last <- if (length(oi_v)) oi_v[[length(oi_v)]] else NA_real_
    close <- close_v[[length(close_v)]]
  }
  list(
    open = suppressWarnings(as.numeric(cash$cash_open[[1]])),
    close = close,
    low = close_v[[i]],
    low_month = as.character(cash$periode[[i]]),
    under = sum(close_v < fl, na.rm = TRUE),
    oi = oi_fy,
    oi_fy = oi_fy,
    oi_last = oi_last,
    ye_month = ye_month
  )
}
