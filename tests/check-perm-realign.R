# SPDX-License-Identifier: AGPL-3.0-or-later
# Copyright (C) 2026 Julio Bauer Cantuaria
# The circular-shift null must not contain shifts that the lag search undoes.
# A shift k with k <= w or k >= n - w (w = widest lag) realigns the series.
root <- "."
setwd(root)
source("tests/helpers.R", encoding = "UTF-8")
fpa_test_bootstrap()

base <- fpa_skip_if_no_optional_data("permanent realign checks")
config <- fpa_load_config(file.path(base, "fpa-config.json"))
built <- fpa_driver_bottom_build(config)
wide <- built$outcome
series <- built$series
closed <- wide$periode[wide$periode <= format(Sys.Date(), "%Y-%m")]

rank_of <- function(name, line, exclude) {
  vec <- series[[name]]
  tgt <- stats::setNames(abs(as.numeric(wide[[line]])), wide$periode)
  if (identical(line, "OI")) tgt <- stats::setNames(as.numeric(wide$OI), wide$periode)
  key <- sort(intersect(intersect(names(vec), names(tgt)), closed))
  key <- key[is.finite(vec[key]) & is.finite(tgt[key])]
  d <- fpa_parse_period(key)
  x <- fpa_stats_pretreat(as.numeric(vec[key]), d)$x
  y <- fpa_stats_pretreat(as.numeric(tgt[key]), d)$x
  s <- fpa_stats_lag_search(x, y)
  n <- length(x)
  set.seed(21L)
  pool <- seq_len(n - 1L)
  if (exclude) pool <- pool[pool > 3L & pool < n - 3L]
  shifts <- pool[sample.int(length(pool), 2000L, replace = TRUE)]
  null_max <- vapply(shifts, function(k) {
    xs <- fpa_stats_circ_shift(x, k)
    max(vapply(-3:3, function(h) abs(fpa_stats_cor(fpa_stats_shift(xs, h), y)$r), numeric(1)), na.rm = TRUE)
  }, numeric(1))
  mean(null_max >= abs(s$r_at_lag) - 1e-12)
}

cases <- list(
  c("taux_utilisation", "REV"), c("tarif", "REV"), c("nuitees_vendues", "REV"),
  c("effectif", "PAYROLL"), c("taux_utilisation", "OI"), c("tarif", "OI"),
  c("nuitees_vendues", "OI"), c("effectif", "OI"), c("temperature_c", "REV"),
  c("precipitations_mm", "REV"), c("temperature_c", "OI"), c("precipitations_mm", "OI")
)
out <- do.call(rbind, lapply(cases, function(cs) data.frame(
  serie = cs[[1]], ligne = cs[[2]],
  rang_actuel = round(rank_of(cs[[1]], cs[[2]], FALSE), 4),
  rang_corrige = round(rank_of(cs[[1]], cs[[2]], TRUE), 4)
)))
print(out, row.names = FALSE)
