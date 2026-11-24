# SPDX-License-Identifier: AGPL-3.0-or-later
# Copyright (C) 2026 Julio Bauer Cantuaria
# fpa-driver-stats.R — Evidence, Independence, and honest timing.
# LAST CHANGE: 2026-09-21
# Axes 1 and 2 are inference on residuals. They do not produce a euro number.
# A textbook p-value is not the decision: the permutation rank of the window maximum is.

#' Pearson r with the critical |r| at the real degrees of freedom.
fpa_stats_cor <- function(x, y, alpha = 0.05) {
  ok <- is.finite(x) & is.finite(y)
  n <- sum(ok)
  out <- list(
    r = NA_real_, n = as.integer(n), df = NA_integer_,
    r_crit = NA_real_, p = NA_real_, significant = FALSE
  )
  if (n < 3L) return(out)
  if (stats::sd(x[ok]) < 1e-12 || stats::sd(y[ok]) < 1e-12) return(out)
  r <- suppressWarnings(stats::cor(x[ok], y[ok]))
  if (!is.finite(r)) return(out)
  df <- n - 2L
  tc <- stats::qt(1 - alpha / 2, df)
  out$r <- r
  out$df <- as.integer(df)
  out$r_crit <- sqrt(tc^2 / (tc^2 + df))
  tstat <- if (abs(r) >= 1 - 1e-12) Inf else abs(r) * sqrt(df / (1 - r^2))
  out$p <- if (is.finite(tstat)) 2 * stats::pt(-tstat, df) else 0
  out$significant <- abs(r) >= out$r_crit
  out
}

#' Partial correlation of x and y given z. Missing z is not a zero.
fpa_stats_partial <- function(x, y, z, alpha = 0.05) {
  if (is.null(z)) {
    return(list(
      r = NA_real_, n = 0L, df = NA_integer_, r_crit = NA_real_,
      p = NA_real_, significant = FALSE, status = "not_available"
    ))
  }
  ok <- is.finite(x) & is.finite(y) & is.finite(z)
  n <- sum(ok)
  out <- list(
    r = NA_real_, n = as.integer(n), df = NA_integer_, r_crit = NA_real_,
    p = NA_real_, significant = FALSE, status = "not_available"
  )
  if (n < 5L) return(out)
  if (stats::sd(z[ok]) < 1e-12) return(out)
  rx <- stats::residuals(stats::lm(x[ok] ~ z[ok]))
  ry <- stats::residuals(stats::lm(y[ok] ~ z[ok]))
  fit <- fpa_stats_cor(rx, ry, alpha = alpha)
  # cor() above used df = n-2 on residuals. The partial test spends one more df.
  df <- n - 3L
  if (df < 1L || !is.finite(fit$r)) return(out)
  tc <- stats::qt(1 - alpha / 2, df)
  out$r <- fit$r
  out$df <- as.integer(df)
  out$r_crit <- sqrt(tc^2 / (tc^2 + df))
  out$n <- as.integer(n)
  tstat <- if (abs(fit$r) >= 1 - 1e-12) Inf else abs(fit$r) * sqrt(df / (1 - fit$r^2))
  out$p <- if (is.finite(tstat)) 2 * stats::pt(-tstat, df) else 0
  out$significant <- abs(fit$r) >= out$r_crit
  out$status <- "measured"
  out
}

#' Subtract calendar-month means. Needs two observations in a month to move it.
fpa_stats_deseason <- function(x, dates) {
  x <- as.numeric(x)
  dates <- as.Date(dates)
  out <- x
  if (!length(x) || all(is.na(dates))) return(out)
  month <- as.integer(format(dates, "%m"))
  for (k in 1:12) {
    idx <- which(month == k & is.finite(x))
    if (length(idx) >= 2L) out[idx] <- x[idx] - mean(x[idx])
  }
  out
}

#' Linear time residual. Does not drop flagged outliers.
fpa_stats_detrend <- function(x) {
  x <- as.numeric(x)
  ok <- is.finite(x)
  if (sum(ok) < 4L || stats::sd(x[ok]) < 1e-12) return(x)
  fit <- stats::lm(x[ok] ~ seq_along(x)[ok])
  x[ok] <- stats::residuals(fit)
  x
}

#' Level or log, then deseason, then detrend. Outliers stay in the series.
fpa_stats_pretreat <- function(x, dates, level = c("level", "log")) {
  level <- match.arg(level)
  x <- as.numeric(x)
  if (identical(level, "log")) {
    if (any(is.finite(x) & x <= 0)) {
      level <- "level"
    } else {
      x[is.finite(x)] <- log(x[is.finite(x)])
    }
  }
  x <- fpa_stats_deseason(x, dates)
  x <- fpa_stats_detrend(x)
  list(x = x, level = level, outliers = fpa_profile_outliers(x))
}

#' h > 0: driver leads the target by h (driver[t-h] with target[t]).
#' h < 0 is a diagnostic: the driver follows the target.
fpa_stats_shift <- function(x, h) {
  x <- as.numeric(x)
  h <- as.integer(h)
  n <- length(x)
  if (!n || h == 0L) return(x)
  if (abs(h) >= n) return(rep(NA_real_, n))
  if (h > 0L) return(c(rep(NA_real_, h), x[seq_len(n - h)]))
  k <- abs(h)
  c(x[(k + 1L):n], rep(NA_real_, k))
}

fpa_stats_circ_shift <- function(x, k) {
  x <- as.numeric(x)
  n <- length(x)
  if (!n) return(x)
  k <- as.integer(k) %% n
  if (k == 0L) return(x)
  c(x[(k + 1L):n], x[seq_len(k)])
}

#' Association at every lag in a signed window. tests_performed is the width.
fpa_stats_lag_search <- function(x, y, window = c(-3L, 3L), min_pairs = 8L, alpha = 0.05) {
  lags <- seq.int(as.integer(window[[1]]), as.integer(window[[2]]))
  rows <- lapply(lags, function(h) {
    fit <- fpa_stats_cor(fpa_stats_shift(x, h), y, alpha = alpha)
    data.frame(
      lag = h, r = fit$r, n = fit$n, r_crit = fit$r_crit,
      significant = isTRUE(fit$significant),
      stringsAsFactors = FALSE
    )
  })
  prof <- do.call(rbind, rows)
  rownames(prof) <- NULL
  eligible <- which(is.finite(prof$r) & prof$n >= min_pairs)
  best <- if (length(eligible)) eligible[[which.max(abs(prof$r[eligible]))]] else NA_integer_
  r0 <- prof$r[prof$lag == 0L]
  if (!length(r0)) r0 <- NA_real_
  lag_selected <- if (is.na(best)) NA_integer_ else as.integer(prof$lag[[best]])
  lag_status <- "not_distinguishable"
  if (!is.na(best)) {
    gain <- abs(prof$r[[best]]) - abs(r0)
    if (!is.na(lag_selected) && lag_selected < 0L && is.finite(gain) && gain >= 0.10) {
      lag_status <- "refused_reverse_causality"
    } else if (isTRUE(prof$significant[[best]])) {
      lag_status <- "suggested"
    }
  }
  list(
    profile = prof,
    lag_selected = lag_selected,
    r_at_lag = if (is.na(best)) NA_real_ else prof$r[[best]],
    r0 = r0,
    lag_status = lag_status,
    tests_performed = nrow(prof)
  )
}

#' Circular-shift null of the maximum |r| inside the window.
#' Returns the rank of the observed maximum (share of random timings at least as large)
#' and a per-lag 95% band so the chart can be redrawn without another search.
#' A shift k within the lag window of either end is undone by the lag search
#' (k <= w or k >= n - w), so it rebuilds the real alignment and is left out.
fpa_stats_permute_pool <- function(n, window = c(-3L, 3L)) {
  w <- max(abs(as.integer(window)))
  pool <- seq_len(max(0L, n - 1L))
  kept <- pool[pool > w & pool < n - w]
  if (length(kept)) kept else pool
}

fpa_stats_permute <- function(x, y, window = c(-3L, 3L), n_perm = 2000L,
                              min_pairs = 8L, seed = 21L) {
  lags <- seq.int(as.integer(window[[1]]), as.integer(window[[2]]))
  n_perm <- as.integer(n_perm)
  set.seed(seed)
  n <- length(x)
  null_abs <- matrix(NA_real_, nrow = n_perm, ncol = length(lags))
  if (n >= 4L && n_perm > 0L) {
    pool <- fpa_stats_permute_pool(n, window)
    shifts <- pool[sample.int(length(pool), n_perm, replace = TRUE)]
    for (i in seq_len(n_perm)) {
      xs <- fpa_stats_circ_shift(x, shifts[[i]])
      for (j in seq_along(lags)) {
        fit <- fpa_stats_cor(fpa_stats_shift(xs, lags[[j]]), y)
        if (fit$n >= min_pairs) null_abs[i, j] <- abs(fit$r)
      }
    }
  }
  null_max <- apply(null_abs, 1, function(row) {
    if (all(!is.finite(row))) NA_real_ else max(row, na.rm = TRUE)
  })
  null_p95 <- apply(null_abs, 2, function(col) {
    col <- col[is.finite(col)]
    if (!length(col)) NA_real_ else as.numeric(stats::quantile(col, 0.95))
  })
  list(null_max = null_max, null_p95 = null_p95, lags = lags)
}

fpa_stats_rank <- function(observed_abs, null_max) {
  null_max <- null_max[is.finite(null_max)]
  if (!length(null_max) || !is.finite(observed_abs)) {
    return(list(rank = NA_real_, best_null = NA_real_, sentence = "Permutation non calculee."))
  }
  rank <- mean(null_max >= observed_abs - 1e-12)
  best_null <- max(null_max)
  pct <- round(100 * rank)
  sentence <- if (rank <= 0.05) {
    paste0(
      "Contre ", length(null_max),
      fpa_txt("loc_redate_in"),
      max(1, pct), fpa_txt("loc_percent_best"),
      format(round(best_null, 2), nsmall = 2),
      ". Le votre atteint ", format(round(observed_abs, 2), nsmall = 2), "."
    )
  } else {
    paste0(
      round(rank * length(null_max)), " re-datages sur ", length(null_max),
      " atteignent au moins ce niveau. Ce decalage ne se distingue pas du hasard."
    )
  }
  list(rank = rank, best_null = best_null, sentence = sentence)
}

#' One-step holdout against seasonal naive (same month last year).
#' Skill above zero means the lagged driver beat that baseline. NA if history is short.
fpa_stats_holdout <- function(driver, target, dates, lag, holdout_n = 12L) {
  n <- length(target)
  empty <- list(
    skill = NA_real_,
    points = data.frame(
      periode = character(), actual = numeric(), predicted = numeric(),
      seasonal_naive = numeric(), stringsAsFactors = FALSE
    ),
    reason = fpa_txt("loc_need_24_holdout")
  )
  if (n < 24L) return(empty)
  h <- if (is.na(lag)) 0L else as.integer(lag)
  x <- fpa_stats_shift(driver, h)
  test <- (n - holdout_n + 1L):n
  train <- seq_len(n - holdout_n)
  ok_tr <- is.finite(x[train]) & is.finite(target[train])
  if (sum(ok_tr) < 8L) {
    empty$reason <- fpa_txt("loc_holdout_few_points")
    return(empty)
  }
  fit <- stats::lm(target[train][ok_tr] ~ x[train][ok_tr])
  pred <- rep(NA_real_, n)
  naive <- rep(NA_real_, n)
  for (t in test) {
    if (t > 12L && is.finite(target[[t - 12L]])) naive[[t]] <- target[[t - 12L]]
    if (is.finite(x[[t]])) {
      pred[[t]] <- unname(coef(fit)[[1]] + coef(fit)[[2]] * x[[t]])
    }
  }
  ok <- is.finite(target[test]) & is.finite(pred[test]) & is.finite(naive[test])
  if (sum(ok) < 3L) {
    empty$reason <- "Holdout incomplet (naif saisonnier ou prediction manquante)."
    return(empty)
  }
  mse_m <- mean((target[test][ok] - pred[test][ok])^2)
  mse_n <- mean((target[test][ok] - naive[test][ok])^2)
  skill <- if (mse_n < 1e-12) NA_real_ else 1 - mse_m / mse_n
  periode <- format(as.Date(dates), "%Y-%m")
  list(
    skill = skill,
    points = data.frame(
      periode = periode[test],
      actual = target[test],
      predicted = pred[test],
      seasonal_naive = naive[test],
      stringsAsFactors = FALSE
    ),
    reason = ""
  )
}

#' Collapse a series to calendar months with the proposed aggregation.
fpa_stats_aggregate_month <- function(dates, values, rule = "mean") {
  dates <- as.Date(dates)
  values <- as.numeric(values)
  ok <- !is.na(dates)
  if (!any(ok)) return(list(dates = as.Date(character(0)), values = numeric(0)))
  key <- format(dates[ok], "%Y-%m")
  values <- values[ok]
  keys <- unique(key)
  out <- vapply(keys, function(k) {
    v <- values[key == k]
    v <- v[is.finite(v)]
    if (!length(v)) return(NA_real_)
    if (identical(rule, "sum") || identical(rule, "working_day")) return(sum(v))
    if (identical(rule, "closing")) return(tail(v, 1))
    if (identical(rule, "count_above")) return(sum(v > 0))
    mean(v)
  }, numeric(1))
  list(dates = fpa_parse_period(keys), values = out)
}

#' Inner join on calendar month. Target dates define the row order.
fpa_stats_align <- function(driver_dates, driver_values, target_dates, target_values,
                            control_values = NULL) {
  kd <- format(as.Date(driver_dates), "%Y-%m")
  kt <- format(as.Date(target_dates), "%Y-%m")
  idx <- match(kt, kd)
  drv <- driver_values[idx]
  ctl <- if (is.null(control_values)) rep(NA_real_, length(kt)) else {
    # control is already on the target calendar when passed that way;
    # if lengths match target, use as-is, else match was done by the caller.
    if (length(control_values) == length(kt)) control_values else rep(NA_real_, length(kt))
  }
  data.frame(
    periode = kt,
    date = as.Date(target_dates),
    driver = as.numeric(drv),
    target = as.numeric(target_values),
    control = as.numeric(ctl),
    stringsAsFactors = FALSE
  )
}

#' Full evaluation of one aligned driver against one target.
#' level is "level" or "log". control NULL means Independence is not available.
fpa_stats_evaluate <- function(driver, target, dates, control = NULL,
                               window = c(-3L, 3L), n_perm = 2000L,
                               min_pairs = 8L, seed = 21L,
                               level = c("level", "log")) {
  level <- match.arg(level)
  dates <- as.Date(dates)
  pre_d <- fpa_stats_pretreat(driver, dates, level = level)
  pre_t <- fpa_stats_pretreat(target, dates, level = "level")
  pre_c <- if (is.null(control)) NULL else fpa_stats_pretreat(control, dates, level = "level")$x
  search <- fpa_stats_lag_search(pre_d$x, pre_t$x, window = window, min_pairs = min_pairs)
  perm <- fpa_stats_permute(
    pre_d$x, pre_t$x, window = window, n_perm = n_perm,
    min_pairs = min_pairs, seed = seed
  )
  obs <- if (is.finite(search$r_at_lag)) abs(search$r_at_lag) else NA_real_
  ranked <- fpa_stats_rank(obs, perm$null_max)
  # Evidence at the selected lag, on residuals.
  evid <- fpa_stats_cor(fpa_stats_shift(pre_d$x, search$lag_selected %||% 0L), pre_t$x)
  indep <- fpa_stats_partial(
    fpa_stats_shift(pre_d$x, search$lag_selected %||% 0L),
    pre_t$x,
    if (is.null(pre_c)) NULL else fpa_stats_shift(pre_c, 0L)
  )
  hold <- fpa_stats_holdout(pre_d$x, pre_t$x, dates, search$lag_selected %||% 0L)
  profile <- search$profile
  profile$null_p95 <- perm$null_p95
  list(
    profile = profile,
    lag_selected = search$lag_selected,
    lag_status = search$lag_status,
    tests_performed = search$tests_performed,
    evidence = evid,
    independence = indep,
    permutation_rank = ranked$rank,
    permutation_iterations = as.integer(n_perm),
    permutation_sentence = ranked$sentence,
    best_null = ranked$best_null,
    oos_skill = hold$skill,
    oos_points = hold$points,
    oos_reason = hold$reason,
    driver_resid = pre_d$x,
    target_resid = pre_t$x,
    outlier_n = sum(pre_d$outliers)
  )
}

#' Pairwise |r| on pretreated series. Groups above the threshold share one id.
fpa_stats_redundancy <- function(resid_list, threshold = 0.85) {
  ids <- names(resid_list)
  group <- setNames(rep("", length(ids)), ids)
  if (length(ids) < 2L) return(group)
  parent <- seq_along(ids)
  find <- function(i) {
    while (parent[[i]] != i) i <- parent[[i]]
    i
  }
  for (i in seq_along(ids)) {
    for (j in seq_along(ids)) {
      if (j <= i) next
      a <- resid_list[[i]]
      b <- resid_list[[j]]
      n <- min(length(a), length(b))
      if (n < 8L) next
      fit <- fpa_stats_cor(a[seq_len(n)], b[seq_len(n)])
      if (is.finite(fit$r) && abs(fit$r) >= threshold) {
        parent[[find(j)]] <- find(i)
      }
    }
  }
  roots <- vapply(seq_along(ids), find, integer(1))
  k <- 0L
  seen <- integer(0)
  for (i in seq_along(ids)) {
    members <- which(roots == roots[[i]])
    if (length(members) < 2L) next
    if (roots[[i]] %in% seen) next
    seen <- c(seen, roots[[i]])
    k <- k + 1L
    group[ids[members]] <- paste0("red-", k)
  }
  group
}
