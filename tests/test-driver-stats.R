# SPDX-License-Identifier: AGPL-3.0-or-later
# Copyright (C) 2026 Julio Bauer Cantuaria
# Timing traps: noise, false season, reverse causality, redundancy, partial vs missing.
source("tests/helpers.R", encoding = "UTF-8")
fpa_test_bootstrap()

month_dates <- function(n = 36L, start = as.Date("2023-01-31")) {
  seq(start, by = "month", length.out = n)
}

# --- no relationship: rank consistent with chance, not a finding ---
set.seed(42)
n <- 36L
dates <- month_dates(n)
null_fit <- fpa_stats_evaluate(
  rnorm(n), rnorm(n), dates,
  window = c(-3L, 3L), n_perm = 400L, seed = 42L
)
stopifnot(is.finite(null_fit$permutation_rank), null_fit$permutation_rank > 0.05)
stopifnot(!identical(
  fpa_driver_evidence_class(
    "independent", n, null_fit$permutation_rank, null_fit$oos_skill,
    null_fit$lag_status, isTRUE(null_fit$evidence$significant)
  ),
  "statistically_validated"
))

# --- two seasonal series, phase-shifted, no causal link ---
idx <- 0:(n - 1L)
season_d <- sin(2 * pi * (idx %% 12) / 12)
season_t <- sin(2 * pi * ((idx + 3) %% 12) / 12)
set.seed(2)
seas <- fpa_stats_evaluate(
  season_d * 10 + rnorm(n, sd = 0.02),
  season_t * 10 + rnorm(n, sd = 0.02),
  dates,
  window = c(-3L, 3L), n_perm = 400L, seed = 2L
)
stopifnot(!identical(seas$lag_status, "confirmed"))
stopifnot(!identical(
  fpa_driver_evidence_class(
    "independent", n, seas$permutation_rank, seas$oos_skill,
    seas$lag_status, isTRUE(seas$evidence$significant)
  ),
  "statistically_validated"
))

# --- driver follows the target: peak at a negative lag ---
set.seed(3)
y <- as.numeric(stats::arima.sim(list(ar = 0.3), n = n))
x <- rep(NA_real_, n)
x[3:n] <- y[1:(n - 2L)]
rev_fit <- fpa_stats_evaluate(
  x, y, dates, window = c(-3L, 3L), n_perm = 200L, seed = 3L
)
stopifnot(identical(rev_fit$lag_status, "refused_reverse_causality"))
stopifnot(is.finite(rev_fit$lag_selected), rev_fit$lag_selected < 0L)

# --- Independence is measured zero, not "not available", when volume did the work ---
set.seed(9)
z <- rnorm(n)
x <- z + rnorm(n, sd = 0.05)
y <- z + rnorm(n, sd = 0.05)
partial <- fpa_stats_partial(x, y, z)
missing <- fpa_stats_partial(x, y, NULL)
stopifnot(identical(partial$status, "measured"))
stopifnot(is.finite(partial$r), abs(partial$r) < 0.2)
stopifnot(identical(missing$status, "not_available"))
stopifnot(is.na(missing$r))

# --- the null never holds a shift the lag search undoes ---
pool <- fpa_stats_permute_pool(37L, c(-3L, 3L))
stopifnot(min(pool) == 4L, max(pool) == 33L, length(pool) == 30L)
stopifnot(length(fpa_stats_permute_pool(6L, c(-3L, 3L))) == 5L)

# --- a real same-month link on 37 months can rank below 5% ---
set.seed(11)
n37 <- 37L
z <- as.numeric(stats::arima.sim(list(ar = 0.4), n = n37))
strong <- fpa_stats_evaluate(
  z + rnorm(n37, sd = 0.2), z, month_dates(n37),
  window = c(-3L, 3L), n_perm = 400L, seed = 11L
)
stopifnot(strong$permutation_rank <= 0.01)

# --- near-duplicate residuals share one redundancy group ---
a <- rnorm(n)
groups <- fpa_stats_redundancy(list(left = a, right = a + rnorm(n, sd = 0.01), other = rnorm(n)))
stopifnot(nzchar(groups[["left"]]), identical(groups[["left"]], groups[["right"]]))
stopifnot(!nzchar(groups[["other"]]))

message("PASS driver-stats")
