# SPDX-License-Identifier: AGPL-3.0-or-later
# Copyright (C) 2026 Julio Bauer Cantuaria
# fpa-objectif.R — Year-end target: manual levers or auto mixes, reverse solve.
# LAST CHANGE: 2026-09-30 — fiscal close is the next one; operating income read for the year and for the close month
# Closed months stay locked (line scales hit forecast only). The answer is a
# mathematical condition on the chosen levers, not proof that the market will move.

FPA_OBJ_METRICS <- c("cash_ye", "oi_fy", "oi_ye")

fpa_fy_end_config <- function(config) {
  co <- config$company
  fy <- suppressWarnings(as.integer(
    if (!is.null(co) && !is.null(co$fiscal_year_end_month)) co$fiscal_year_end_month
    else config$fiscal_year_end_month %||% config$fy_end_month %||% 12L
  ))
  if (length(fy) != 1L || !is.finite(fy) || fy < 1L || fy > 12L) fy <- 12L
  fy
}

#' "2026-12" -> "décembre 2026".
fpa_ym_fr <- function(ym) {
  ym <- as.character(ym %||% "")
  if (!grepl("^[0-9]{4}-[0-9]{2}$", ym)) return(ym)
  paste(fpa_chain_month_fr(as.integer(substr(ym, 6L, 7L))), substr(ym, 1L, 4L))
}

#' The twelve YYYY-MM months that end on a fiscal close.
fpa_fy_months <- function(close) {
  close <- as.character(close %||% "")
  if (!grepl("^[0-9]{4}-[0-9]{2}$", close)) return(character(0))
  y <- as.integer(substr(close, 1L, 4L))
  m <- as.integer(substr(close, 6L, 7L))
  idx <- (y * 12L + m - 1L) - 11:0
  sprintf("%04d-%02d", idx %/% 12L, idx %% 12L + 1L)
}

#' Next fiscal close on or after the last closed month, and the month that can
#' actually be read. The forecast horizon is not extended: when the close lies
#' past the last month, `beyond` is TRUE and `measured` is that last month.
fpa_year_end_info <- function(config, status = NULL, periods = character()) {
  closed <- ""
  if (!is.null(status) && nrow(status) && all(c("periode", "status") %in% names(status))) {
    closed <- tryCatch(as.character(fpa_health_closed_month(status) %||% ""), error = function(e) "")
  }
  if (!grepl("^[0-9]{4}-[0-9]{2}$", closed)) closed <- format(Sys.Date(), "%Y-%m")
  fy <- fpa_fy_end_config(config)
  y <- as.integer(substr(closed, 1L, 4L))
  m <- as.integer(substr(closed, 6L, 7L))
  close <- sprintf("%04d-%02d", if (m > fy) y + 1L else y, fy)
  pers <- sort(unique(as.character(periods %||% character(0))))
  pers <- pers[grepl("^[0-9]{4}-[0-9]{2}$", pers)]
  measured <- close
  beyond <- FALSE
  if (length(pers) && !close %in% pers) {
    before <- pers[pers <= close]
    measured <- if (length(before)) before[[length(before)]] else pers[[length(pers)]]
    beyond <- close > pers[[length(pers)]]
  }
  fy_all <- fpa_fy_months(close)
  list(
    close = close, measured = measured, beyond = beyond, fy_end = fy,
    months = if (length(pers)) intersect(fy_all, pers) else fy_all,
    fy_start = fy_all[[1]]
  )
}

#' Fiscal close (YYYY-MM) that the year-end metrics read. Passed to the solvers.
fpa_year_end_month <- function(config, status = NULL, periods = character()) {
  fpa_year_end_info(config, status, periods)$close
}

fpa_ye_index <- function(pers, ye_month) {
  pers <- as.character(pers)
  if (!length(pers)) return(NA_integer_)
  i <- if (nzchar(ye_month)) match(ye_month, pers) else NA_integer_
  if (is.na(i) && nzchar(ye_month)) {
    ok <- which(pers <= ye_month)
    if (length(ok)) i <- ok[[length(ok)]]
  }
  if (is.na(i)) i <- length(pers)
  i
}

#' Read a year-end metric from a run. ye_month is the fiscal close.
#' cash_ye: cash balance at the close (or the last month read before it).
#' oi_ye: operating income of that one month.
#' oi_fy: operating income summed over the fiscal-year months present.
fpa_objectif_read <- function(res, metric = FPA_OBJ_METRICS, ye_month) {
  metric <- match.arg(metric, FPA_OBJ_METRICS)
  ye_month <- as.character(ye_month %||% "")
  if (identical(metric, "cash_ye")) {
    cash <- res$cash
    if (is.null(cash) || !nrow(cash) || !"cash_close" %in% names(cash)) return(NA_real_)
    i <- fpa_ye_index(cash$periode, ye_month)
    return(suppressWarnings(as.numeric(cash$cash_close[[i]])))
  }
  pnl <- res$pnl
  if (is.null(pnl) || !nrow(pnl) || !"OI" %in% names(pnl)) return(NA_real_)
  pers <- as.character(pnl$periode)
  oi <- suppressWarnings(as.numeric(pnl$OI))
  if (identical(metric, "oi_fy")) {
    months <- fpa_fy_months(ye_month)
    keep <- if (length(months)) pers %in% months else rep(TRUE, length(pers))
    if (!any(keep)) return(NA_real_)
    return(sum(oi[keep], na.rm = TRUE))
  }
  oi[[fpa_ye_index(pers, ye_month)]]
}

#' Percent gap of a typed target versus the current projection.
#' Positive means the target sits above the projection. The base is taken in
#' absolute value so a negative projection does not flip the direction.
#' Undefined when the base is ~0.
fpa_objectif_pct <- function(target, base) {
  target <- suppressWarnings(as.numeric(target))
  base <- suppressWarnings(as.numeric(base))
  if (!is.finite(target) || !is.finite(base) || abs(base) < 0.5) return(NA_real_)
  100 * (target - base) / abs(base)
}

#' Discrete deltas a slider can land on, always including zero.
fpa_objectif_grid <- function(min_d, max_d, step) {
  min_d <- suppressWarnings(as.numeric(min_d))
  max_d <- suppressWarnings(as.numeric(max_d))
  step <- suppressWarnings(as.numeric(step))
  if (!is.finite(min_d) || !is.finite(max_d) || !is.finite(step) || step <= 0) {
    return(0)
  }
  if (min_d > max_d) {
    tmp <- min_d
    min_d <- max_d
    max_d <- tmp
  }
  sort(unique(c(0, seq(min_d, max_d, by = step))))
}

#' P&L lines are stored on a 5-point tick. A few hundred euros of year-end cash
#' sits between those ticks, so the search uses a 1-point step.
fpa_objectif_line_step <- function(step) {
  step <- suppressWarnings(as.numeric(step))
  if (!is.finite(step) || step <= 0) return(1)
  min(step, 1)
}

#' Snap a continuous move onto the nearest dial step.
fpa_objectif_snap <- function(delta, min_d, max_d, step) {
  grid <- fpa_objectif_grid(min_d, max_d, step)
  if (!length(grid)) return(0)
  delta <- suppressWarnings(as.numeric(delta))
  if (!is.finite(delta)) return(0)
  grid[[which.min(abs(grid - delta))]]
}

#' Parse UI tokens "assum:id" / "line:id" into lever rows with dial bounds.
fpa_objectif_parse_levers <- function(tokens, lv) {
  tokens <- unique(as.character(tokens %||% character(0)))
  tokens <- tokens[nzchar(tokens) & grepl("^(assum|line):", tokens)]
  out <- list()
  for (tok in tokens) {
    kind <- sub(":.*$", "", tok)
    id <- sub("^(assum|line):", "", tok)
    if (identical(kind, "assum")) {
      if (!id %in% (lv$ids %||% character(0))) next
      spec <- fpa_scenario_lever_spec(id, fpa_scenario_find(lv$keys, id))
      out[[length(out) + 1L]] <- list(
        token = tok, id = id, kind = "assum",
        min = spec$min, max = spec$max, step = spec$step
      )
    } else {
      if (!id %in% (lv$lines %||% character(0))) next
      range <- lv$line_spec[[id]] %||% list(min = -20, max = 20, step = 5)
      out[[length(out) + 1L]] <- list(
        token = tok, id = id, kind = "line",
        min = range$min, max = range$max, step = fpa_objectif_line_step(range$step)
      )
    }
  }
  out
}

#' Merge solved deltas into board assum/lines lists.
fpa_objectif_apply_deltas <- function(assum, lines, deltas) {
  assum <- assum %||% list()
  lines <- lines %||% list()
  for (nm in names(deltas$assum %||% list())) assum[[nm]] <- deltas$assum[[nm]]
  for (nm in names(deltas$lines %||% list())) lines[[nm]] <- deltas$lines[[nm]]
  list(assum = assum, lines = lines)
}

fpa_objectif_empty <- function(reason, metric, ye_month, evals = 0L, base = NA_real_) {
  list(
    ok = FALSE, reason = reason, deltas = list(assum = list(), lines = list()),
    value = NA_real_, base = base, ye_month = ye_month, metric = metric,
    evals = as.integer(evals), gap = NA_real_,
    reachable_lo = NA_real_, reachable_hi = NA_real_
  )
}

#' Reverse solve for one or more levers.
#' levers: list of lists from fpa_objectif_parse_levers.
#' One lever uses a dial-grid search. Several levers share one effort t in [0, 1]
#' toward each lever's improving extreme (joint combination).
fpa_objectif_solve <- function(config_path, base_values, keys, levers,
                               target, metric = FPA_OBJ_METRICS,
                               ye_month, status = NULL,
                               other_assum = list(), other_lines = list(),
                               tol = 50, max_evals = 14L) {
  metric <- match.arg(metric, FPA_OBJ_METRICS)
  target <- suppressWarnings(as.numeric(target))
  if (!is.finite(target)) {
    return(fpa_objectif_empty("bad_target", metric, ye_month))
  }
  if (!length(levers)) {
    return(fpa_objectif_empty("no_lever", metric, ye_month))
  }
  if (length(levers) == 1L) {
    return(fpa_objectif_solve_one(
      config_path, base_values, keys, levers[[1]],
      target, metric, ye_month, other_assum, other_lines, tol, max_evals
    ))
  }
  fpa_objectif_solve_many(
    config_path, base_values, keys, levers,
    target, metric, ye_month, other_assum, other_lines, tol, max_evals
  )
}

fpa_objectif_solve_one <- function(config_path, base_values, keys, lever,
                                   target, metric, ye_month,
                                   other_assum, other_lines, tol, max_evals) {
  lever_id <- lever$id
  lever_kind <- lever$kind
  min_d <- lever$min
  max_d <- lever$max
  step <- lever$step
  grid <- fpa_objectif_grid(min_d, max_d, step)
  if (!length(grid)) {
    return(fpa_objectif_empty("bad_grid", metric, ye_month))
  }
  cache <- new.env(parent = emptyenv())
  evals <- 0L
  score_at <- function(delta) {
    key <- as.character(delta)
    if (exists(key, envir = cache, inherits = FALSE)) return(cache[[key]])
    if (evals >= max_evals) return(NA_real_)
    assum <- other_assum
    lines <- other_lines
    if (identical(lever_kind, "assum")) assum[[lever_id]] <- delta else lines[[lever_id]] <- delta
    res <- tryCatch(
      fpa_scenario_run(config_path, base_values, assum, lines, keys),
      error = function(e) NULL
    )
    evals <<- evals + 1L
    v <- if (is.null(res)) NA_real_ else fpa_objectif_read(res, metric, ye_month)
    cache[[key]] <- v
    v
  }
  base_v <- score_at(0)
  pack_deltas <- function(d) {
    if (identical(lever_kind, "assum")) list(assum = stats::setNames(list(d), lever_id), lines = list())
    else list(assum = list(), lines = stats::setNames(list(d), lever_id))
  }
  if (!is.finite(base_v)) {
    out <- fpa_objectif_empty("base_fail", metric, ye_month, evals)
    out$deltas <- pack_deltas(0)
    return(out)
  }
  if (abs(base_v - target) <= tol) {
    return(list(
      ok = TRUE, reason = "already", deltas = pack_deltas(0),
      value = base_v, base = base_v, ye_month = ye_month, metric = metric,
      evals = evals, gap = base_v - target,
      reachable_lo = base_v, reachable_hi = base_v
    ))
  }
  lo <- grid[[1]]
  hi <- grid[[length(grid)]]
  v_lo <- score_at(lo)
  v_hi <- score_at(hi)
  if (!is.finite(v_lo) || !is.finite(v_hi)) {
    out <- fpa_objectif_empty("range_fail", metric, ye_month, evals, base_v)
    return(out)
  }
  band_lo <- min(v_lo, v_hi)
  band_hi <- max(v_lo, v_hi)
  if (target < band_lo - tol || target > band_hi + tol) {
    if (identical(lever_kind, "line")) {
      past <- fpa_objectif_past_line(
        config_path, base_values, keys, lever,
        target, metric, ye_month, other_assum, other_lines,
        tol, base_v
      )
      if (!is.null(past)) return(past)
    }
    return(list(
      ok = FALSE, reason = "infeasible", deltas = list(assum = list(), lines = list()),
      value = NA_real_, base = base_v, ye_month = ye_month, metric = metric,
      evals = evals, gap = NA_real_,
      reachable_lo = band_lo, reachable_hi = band_hi
    ))
  }
  increasing <- v_hi >= v_lo
  left <- 1L
  right <- length(grid)
  best_i <- if (abs(v_lo - target) <= abs(v_hi - target)) 1L else length(grid)
  best_gap <- abs((if (best_i == 1L) v_lo else v_hi) - target)
  while (left <= right && evals < max_evals) {
    mid <- as.integer(floor((left + right) / 2))
    d <- grid[[mid]]
    v <- score_at(d)
    if (!is.finite(v)) break
    gap <- abs(v - target)
    if (gap < best_gap) {
      best_gap <- gap
      best_i <- mid
    }
    if (gap <= tol) {
      best_i <- mid
      break
    }
    if (identical(increasing, TRUE)) {
      if (v < target) left <- mid + 1L else right <- mid - 1L
    } else {
      if (v > target) left <- mid + 1L else right <- mid - 1L
    }
  }
  d_best <- grid[[best_i]]
  v_best <- score_at(d_best)
  list(
    ok = is.finite(v_best),
    reason = if (is.finite(v_best) && abs(v_best - target) <= tol) "hit"
    else if (is.finite(v_best)) "closest"
    else "fail",
    deltas = pack_deltas(d_best),
    value = v_best,
    base = base_v,
    ye_month = ye_month,
    metric = metric,
    evals = evals,
    gap = if (is.finite(v_best)) v_best - target else NA_real_,
    reachable_lo = band_lo,
    reachable_hi = band_hi
  )
}

fpa_objectif_solve_many <- function(config_path, base_values, keys, levers,
                                    target, metric, ye_month,
                                    other_assum, other_lines, tol, max_evals) {
  cache <- new.env(parent = emptyenv())
  evals <- 0L
  skipped <- list()
  stamp <- function(out) {
    out$inert <- lapply(skipped, function(lev) list(kind = lev$kind, id = lev$id))
    out
  }
  score_bundle <- function(deltas) {
    key <- paste(
      paste(names(deltas$assum), unlist(deltas$assum), sep = "=", collapse = ";"),
      paste(names(deltas$lines), unlist(deltas$lines), sep = "=", collapse = ";"),
      sep = "|"
    )
    if (exists(key, envir = cache, inherits = FALSE)) return(cache[[key]])
    if (evals >= max_evals) return(NA_real_)
    board <- fpa_objectif_apply_deltas(other_assum, other_lines, deltas)
    res <- tryCatch(
      fpa_scenario_run(config_path, base_values, board$assum, board$lines, keys),
      error = function(e) NULL
    )
    evals <<- evals + 1L
    v <- if (is.null(res)) NA_real_ else fpa_objectif_read(res, metric, ye_month)
    cache[[key]] <- v
    v
  }
  zero <- list(assum = list(), lines = list())
  base_v <- score_bundle(zero)
  if (!is.finite(base_v)) {
    return(stamp(fpa_objectif_empty("base_fail", metric, ye_month, evals)))
  }
  if (abs(base_v - target) <= tol) {
    return(stamp(list(
      ok = TRUE, reason = "already", deltas = zero,
      value = base_v, base = base_v, ye_month = ye_month, metric = metric,
      evals = evals, gap = base_v - target,
      reachable_lo = base_v, reachable_hi = base_v
    )))
  }
  need_up <- target > base_v
  extremes <- list()
  for (lev in levers) {
    d_lo <- lev$min
    d_hi <- lev$max
    trial_lo <- if (identical(lev$kind, "assum")) {
      list(assum = stats::setNames(list(d_lo), lev$id), lines = list())
    } else {
      list(assum = list(), lines = stats::setNames(list(d_lo), lev$id))
    }
    trial_hi <- if (identical(lev$kind, "assum")) {
      list(assum = stats::setNames(list(d_hi), lev$id), lines = list())
    } else {
      list(assum = list(), lines = stats::setNames(list(d_hi), lev$id))
    }
    v_lo <- score_bundle(trial_lo)
    v_hi <- score_bundle(trial_hi)
    # A missed probe is one lever, not a failed solve. The others still report a result.
    if (!is.finite(v_lo) || !is.finite(v_hi)) {
      skipped[[length(skipped) + 1L]] <- lev
      next
    }
    # A lever that does not move this figure stays out of the mix.
    if (max(abs(v_lo - base_v), abs(v_hi - base_v)) < 1) {
      skipped[[length(skipped) + 1L]] <- lev
      next
    }
    # Pick the extreme that moves the metric toward the target.
    if (need_up) {
      extreme <- if (v_hi >= v_lo) d_hi else d_lo
    } else {
      extreme <- if (v_hi <= v_lo) d_hi else d_lo
    }
    extremes[[length(extremes) + 1L]] <- list(lever = lev, extreme = extreme)
  }
  if (!length(extremes)) {
    return(stamp(list(
      ok = FALSE, reason = "infeasible", deltas = list(assum = list(), lines = list()),
      value = base_v, base = base_v, ye_month = ye_month, metric = metric,
      evals = evals, gap = base_v - target,
      reachable_lo = base_v, reachable_hi = base_v
    )))
  }
  deltas_at <- function(t) {
    t <- max(0, min(1, suppressWarnings(as.numeric(t))))
    assum <- list()
    lines <- list()
    for (row in extremes) {
      lev <- row$lever
      d <- fpa_objectif_snap(t * row$extreme, lev$min, lev$max, lev$step)
      if (identical(lev$kind, "assum")) assum[[lev$id]] <- d else lines[[lev$id]] <- d
    }
    list(assum = assum, lines = lines)
  }
  v_end <- score_bundle(deltas_at(1))
  if (!is.finite(v_end)) {
    return(stamp(list(
      ok = FALSE, reason = "infeasible", deltas = list(assum = list(), lines = list()),
      value = base_v, base = base_v, ye_month = ye_month, metric = metric,
      evals = evals, gap = base_v - target,
      reachable_lo = base_v, reachable_hi = base_v
    )))
  }
  band_lo <- min(base_v, v_end)
  band_hi <- max(base_v, v_end)
  if (target < band_lo - tol || target > band_hi + tol) {
    # Same rule as one line past its dial: cost and revenue lines may leave ±20
    # together, up to -80 / +400, so a mix can still reach the target.
    span <- v_end - base_v
    k <- if (is.finite(span) && abs(span) >= 1) (target - base_v) / span else NA_real_
    best_v <- v_end
    best_bundle <- deltas_at(1)
    if (is.finite(k) && k > 1) {
      bundle_at <- function(kk) {
        assum <- list()
        lines <- list()
        for (row in extremes) {
          lev <- row$lever
          d <- kk * row$extreme
          if (identical(lev$kind, "line")) {
            d <- max(-80, min(400, round(d)))
          } else {
            d <- fpa_objectif_snap(d, lev$min, lev$max, lev$step)
          }
          if (!is.finite(d) || d == 0) next
          if (identical(lev$kind, "assum")) assum[[lev$id]] <- d else lines[[lev$id]] <- d
        }
        list(assum = assum, lines = lines)
      }
      for (i in seq_len(3L)) {
        bundle <- bundle_at(k)
        v <- score_bundle(bundle)
        if (!is.finite(v)) break
        if (abs(v - target) < abs(best_v - target)) {
          best_v <- v
          best_bundle <- bundle
        }
        if (abs(v - target) <= tol) {
          return(stamp(list(
            ok = TRUE, reason = "hit", deltas = bundle,
            value = v, base = base_v, ye_month = ye_month, metric = metric,
            evals = evals, gap = v - target,
            reachable_lo = min(base_v, v), reachable_hi = max(base_v, v)
          )))
        }
        if (abs(v - base_v) < 1) break
        k <- k * (target - base_v) / (v - base_v)
        if (!is.finite(k) || k <= 0) break
        k <- min(k, 40)
      }
    }
    return(stamp(list(
      ok = FALSE, reason = "infeasible", deltas = list(assum = list(), lines = list()),
      value = best_v, base = base_v, ye_month = ye_month, metric = metric,
      evals = evals, gap = best_v - target,
      reachable_lo = min(base_v, best_v), reachable_hi = max(base_v, best_v)
    )))
  }
  # Binary search shared effort t.
  lo_t <- 0
  hi_t <- 1
  best <- list(t = 1, deltas = deltas_at(1), value = v_end)
  best_gap <- abs(v_end - target)
  for (i in seq_len(8L)) {
    if (evals >= max_evals) break
    mid <- (lo_t + hi_t) / 2
    dmid <- deltas_at(mid)
    v <- score_bundle(dmid)
    if (!is.finite(v)) break
    gap <- abs(v - target)
    if (gap < best_gap) {
      best_gap <- gap
      best <- list(t = mid, deltas = dmid, value = v)
    }
    if (gap <= tol) {
      best <- list(t = mid, deltas = dmid, value = v)
      break
    }
    if (need_up) {
      if (v < target) lo_t <- mid else hi_t <- mid
    } else {
      if (v > target) lo_t <- mid else hi_t <- mid
    }
  }
  v_best <- best$value
  stamp(list(
    ok = is.finite(v_best),
    reason = if (is.finite(v_best) && abs(v_best - target) <= tol) "hit"
    else if (is.finite(v_best)) "closest"
    else "fail",
    deltas = best$deltas,
    value = v_best,
    base = base_v,
    ye_month = ye_month,
    metric = metric,
    evals = evals,
    gap = if (is.finite(v_best)) v_best - target else NA_real_,
    reachable_lo = band_lo,
    reachable_hi = band_hi
  ))
}

#' Signature of a lever set for mix de-duplication.
fpa_objectif_set_key <- function(levers) {
  ids <- vapply(levers, function(L) paste0(L$kind, ":", L$id), character(1))
  paste(sort(ids), collapse = "+")
}

#' Rank pool levers by how far their improving extreme moves the metric toward target.
fpa_objectif_rank_pool <- function(config_path, base_values, keys, pool,
                                   target, metric, ye_month,
                                   other_assum, other_lines, max_probe = 48L) {
  if (!length(pool)) return(list())
  zero <- list(assum = list(), lines = list())
  board0 <- fpa_objectif_apply_deltas(other_assum, other_lines, zero)
  res0 <- tryCatch(
    fpa_scenario_run(config_path, base_values, board0$assum, board0$lines, keys),
    error = function(e) NULL
  )
  base_v <- if (is.null(res0)) NA_real_ else fpa_objectif_read(res0, metric, ye_month)
  if (!is.finite(base_v)) return(list())
  need_up <- target > base_v
  scored <- list()
  n <- 0L
  for (lev in pool) {
    if (n >= max_probe) break
    for (d in c(lev$min, lev$max)) {
      trial <- if (identical(lev$kind, "assum")) {
        list(assum = stats::setNames(list(d), lev$id), lines = list())
      } else {
        list(assum = list(), lines = stats::setNames(list(d), lev$id))
      }
      board <- fpa_objectif_apply_deltas(other_assum, other_lines, trial)
      res <- tryCatch(
        fpa_scenario_run(config_path, base_values, board$assum, board$lines, keys),
        error = function(e) NULL
      )
      n <- n + 1L
      v <- if (is.null(res)) NA_real_ else fpa_objectif_read(res, metric, ye_month)
      if (!is.finite(v)) next
      help <- if (need_up) v - base_v else base_v - v
      scored[[length(scored) + 1L]] <- list(lever = lev, extreme = d, help = help, value = v)
    }
  }
  if (!length(scored)) return(list())
  by_id <- list()
  for (row in scored) {
    k <- paste0(row$lever$kind, ":", row$lever$id)
    if (is.null(by_id[[k]]) || row$help > by_id[[k]]$help) by_id[[k]] <- row
  }
  ranked <- by_id[order(vapply(by_id, function(x) x$help, numeric(1)), decreasing = TRUE)]
  Filter(function(x) is.finite(x$help) && x$help > 0, ranked)
}

#' Build up to five distinct lever recipes from a ranked pool.
fpa_objectif_recipes <- function(ranked, n = 5L) {
  levers <- lapply(ranked, function(x) x$lever)
  n_lev <- length(levers)
  if (!n_lev) return(list())
  recipes <- list()
  add <- function(idx) {
    idx <- idx[idx >= 1L & idx <= n_lev]
    if (!length(idx)) return()
    set <- levers[unique(idx)]
    key <- fpa_objectif_set_key(set)
    if (any(vapply(recipes, function(r) identical(r$key, key), logical(1)))) return()
    recipes[[length(recipes) + 1L]] <<- list(key = key, levers = set)
  }
  add(1L)
  if (n_lev >= 2L) add(2L)
  if (n_lev >= 2L) add(c(1L, 2L))
  if (n_lev >= 3L) add(c(1L, 3L))
  if (n_lev >= 3L) add(c(1L, 2L, 3L))
  if (n_lev >= 4L && length(recipes) < n) add(c(2L, 3L))
  if (n_lev >= 4L && length(recipes) < n) add(4L)
  if (length(recipes) > n) recipes <- recipes[seq_len(n)]
  recipes
}

#' A P&L line may pass its dial stop. Costs stop at −80% so the line does not
#' change sign. Revenue may rise to +400%. Returns a hit only when a checked
#' run lands inside tol. NULL when the stop, or that ceiling, cannot reach.
fpa_objectif_past_line <- function(config_path, base_values, keys, lever,
                                   target, metric, ye_month,
                                   other_assum = list(), other_lines = list(),
                                   tol = 50, base_v = NA_real_) {
  if (!identical(lever$kind, "line")) return(NULL)
  metric <- match.arg(metric, FPA_OBJ_METRICS)
  target <- suppressWarnings(as.numeric(target))
  if (!is.finite(target)) return(NULL)
  lo_cap <- -80
  hi_cap <- 400
  score_pct <- function(pct) {
    lines <- other_lines %||% list()
    lines[[lever$id]] <- pct
    res <- tryCatch(
      fpa_scenario_run(
        config_path, base_values, other_assum %||% list(), lines, keys
      ),
      error = function(e) NULL
    )
    if (is.null(res)) return(NA_real_)
    fpa_objectif_read(res, metric, ye_month)
  }
  if (!is.finite(base_v)) base_v <- score_pct(0)
  if (!is.finite(base_v) || abs(base_v - target) <= tol) return(NULL)
  probe <- if (target > base_v) {
    mx <- suppressWarnings(as.numeric(lever$max))
    if (is.finite(mx) && mx > 0) mx else 20
  } else {
    mn <- suppressWarnings(as.numeric(lever$min))
    if (is.finite(mn) && mn < 0) mn else -20
  }
  v_probe <- score_pct(probe)
  if (!is.finite(v_probe) || abs(v_probe - base_v) < 1) return(NULL)
  need <- probe * (target - base_v) / (v_probe - base_v)
  need <- max(lo_cap, min(hi_cap, round(need)))
  dial_min <- suppressWarnings(as.numeric(lever$min %||% -20))
  dial_max <- suppressWarnings(as.numeric(lever$max %||% 20))
  if (is.finite(dial_min) && is.finite(dial_max) && need >= dial_min && need <= dial_max) {
    return(NULL)
  }
  pred <- base_v + (v_probe - base_v) / probe * need
  if (!is.finite(pred) || abs(pred - base_v) + tol < abs(target - base_v)) return(NULL)
  v <- NA_real_
  for (i in seq_len(3L)) {
    v <- score_pct(need)
    if (!is.finite(v)) return(NULL)
    if (abs(v - target) <= tol) break
    if (abs(v - base_v) < 1) return(NULL)
    need <- need * (target - base_v) / (v - base_v)
    need <- max(lo_cap, min(hi_cap, round(need)))
  }
  if (!is.finite(v) || abs(v - target) > tol) return(NULL)
  list(
    ok = TRUE,
    reason = "hit",
    deltas = list(assum = list(), lines = stats::setNames(list(need), lever$id)),
    value = v,
    base = base_v,
    ye_month = ye_month,
    metric = metric,
    evals = 0L,
    gap = v - target,
    reachable_lo = min(base_v, v),
    reachable_hi = max(base_v, v),
    past_dial = TRUE
  )
}

#' Auto mode: probe the dial pool, solve about five mixes, return attainable ones first.
fpa_objectif_auto <- function(config_path, base_values, keys, pool_levers,
                              target, metric = FPA_OBJ_METRICS,
                              ye_month, other_assum = list(), other_lines = list(),
                              tol = 50, n_mixes = 5L, max_evals_each = 10L) {
  metric <- match.arg(metric, FPA_OBJ_METRICS)
  target <- suppressWarnings(as.numeric(target))
  empty <- list(
    ok = FALSE, reason = "no_mix", mixes = list(), base = NA_real_,
    ye_month = ye_month, metric = metric, evals = 0L
  )
  if (!is.finite(target) || !length(pool_levers)) return(empty)
  ranked <- fpa_objectif_rank_pool(
    config_path, base_values, keys, pool_levers,
    target, metric, ye_month, other_assum, other_lines
  )
  if (!length(ranked)) {
    empty$reason <- "no_help"
    return(empty)
  }
  recipes <- fpa_objectif_recipes(ranked, n = as.integer(n_mixes))
  mixes <- list()
  total_evals <- 0L
  best_band <- list(lo = Inf, hi = -Inf)
  for (rec in recipes) {
    got <- fpa_objectif_solve(
      config_path, base_values, keys, rec$levers,
      target = target, metric = metric, ye_month = ye_month,
      other_assum = other_assum, other_lines = other_lines,
      tol = tol, max_evals = as.integer(max_evals_each)
    )
    total_evals <- total_evals + as.integer(got$evals %||% 0L)
    if (is.finite(got$reachable_lo %||% NA_real_)) {
      best_band$lo <- min(best_band$lo, got$reachable_lo)
      best_band$hi <- max(best_band$hi, got$reachable_hi %||% got$reachable_lo)
    }
    if (identical(got$reason, "already")) {
      return(list(
        ok = TRUE, reason = "already", mixes = list(), base = got$base,
        ye_month = ye_month, metric = metric, evals = total_evals
      ))
    }
    if (!got$reason %in% c("hit", "closest")) next
    if (!isTRUE(got$ok) || !is.finite(got$value)) next
    mixes[[length(mixes) + 1L]] <- list(
      levers = rec$levers,
      deltas = got$deltas,
      value = got$value,
      base = got$base,
      gap = got$gap,
      reason = got$reason,
      reachable_lo = got$reachable_lo,
      reachable_hi = got$reachable_hi
    )
  }
  has_hit <- any(vapply(mixes, function(m) identical(m$reason, "hit"), logical(1)))
  if (!has_hit) {
    lines <- Filter(function(L) identical(L$kind, "line"), pool_levers)
    if (length(lines)) {
      lines <- lines[order(vapply(lines, function(L) if (identical(L$id, "REV")) 0 else 1, numeric(1)))]
      base_v <- if (length(mixes) && is.finite(mixes[[1]]$base %||% NA_real_)) mixes[[1]]$base else NA_real_
      for (lev in lines) {
        past <- fpa_objectif_past_line(
          config_path, base_values, keys, lev,
          target, metric, ye_month, other_assum, other_lines,
          tol, base_v
        )
        if (is.null(past)) next
        mixes <- c(list(list(
          levers = list(lev),
          deltas = past$deltas,
          value = past$value,
          base = past$base,
          gap = past$gap,
          reason = "hit",
          reachable_lo = past$reachable_lo,
          reachable_hi = past$reachable_hi,
          past_dial = TRUE
        )), mixes)
        break
      }
    }
  }
  if (!length(mixes)) {
    return(list(
      ok = FALSE, reason = "infeasible", mixes = list(),
      base = NA_real_,
      ye_month = ye_month, metric = metric, evals = total_evals,
      reachable_lo = if (is.finite(best_band$lo)) best_band$lo else NA_real_,
      reachable_hi = if (is.finite(best_band$hi)) best_band$hi else NA_real_
    ))
  }
  ord <- order(
    vapply(mixes, function(m) if (identical(m$reason, "hit")) 0L else 1L, integer(1)),
    vapply(mixes, function(m) abs(m$gap %||% Inf), numeric(1))
  )
  mixes <- mixes[ord]
  if (length(mixes) > n_mixes) mixes <- mixes[seq_len(n_mixes)]
  list(
    ok = TRUE,
    reason = if (any(vapply(mixes, function(m) identical(m$reason, "hit"), logical(1)))) "hit" else "closest",
    mixes = mixes,
    base = mixes[[1]]$base,
    ye_month = ye_month,
    metric = metric,
    evals = total_evals,
    reachable_lo = if (is.finite(best_band$lo)) best_band$lo else NA_real_,
    reachable_hi = if (is.finite(best_band$hi)) best_band$hi else NA_real_
  )
}
