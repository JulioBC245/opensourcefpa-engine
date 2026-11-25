# SPDX-License-Identifier: AGPL-3.0-or-later
# Copyright (C) 2026 Julio Bauer Cantuaria
# Two goals on the same closed months: each series against its own line
# (REV or PAYROLL) and against operating income. Read only: prints the
# comparison with the stored 12-month line test and writes nothing.
root <- "."
setwd(root)
source("tests/helpers.R", encoding = "UTF-8")
fpa_test_bootstrap()

base <- fpa_skip_if_no_optional_data("two-goals checks")
config <- fpa_load_config(file.path(base, "fpa-config.json"))
grades <- utils::read.csv(file.path(base, "work/drivers/driver-grades.csv"), stringsAsFactors = FALSE)
built <- fpa_driver_bottom_build(config)
wide <- built$outcome
series <- built$series
closed <- wide$periode[wide$periode <= format(Sys.Date(), "%Y-%m")]
volume_name <- fpa_driver_bottom_volume_name(names(series))
rate_name <- if ("tarif" %in% names(series)) "tarif" else NA_character_

run_one <- function(name, line) {
  vec <- series[[name]]
  tgt <- stats::setNames(as.numeric(wide[[line]]), wide$periode)
  if (line %in% c("PAYROLL", "COGS", "OPEX_OTHER")) tgt <- abs(tgt)
  key <- sort(intersect(intersect(names(vec), names(tgt)), closed))
  key <- key[is.finite(vec[key]) & is.finite(tgt[key])]
  if (length(key) < 8L) return(NULL)
  control_name <- if (identical(name, volume_name)) rate_name else volume_name
  ctl <- if (!is.na(control_name) && !identical(control_name, name)) as.numeric(series[[control_name]][key]) else NULL
  ev <- fpa_stats_evaluate(as.numeric(vec[key]), as.numeric(tgt[key]), fpa_parse_period(key),
                           control = ctl, window = c(-3L, 3L), n_perm = 2000L, seed = 21L)
  data.frame(n = length(key), from = key[[1]], to = key[[length(key)]],
             lag = ev$lag_selected, r = round(ev$evidence$r, 3),
             crit = round(ev$evidence$r_crit, 3), partial = round(ev$independence$r, 3),
             rank = round(ev$permutation_rank, 4), skill = round(ev$oos_skill, 3))
}

rows <- list()
for (i in seq_len(nrow(grades))) {
  g <- grades[i, ]
  name <- g$series_name
  if (!name %in% names(series)) next
  own <- run_one(name, g$target_line)
  rev <- if (identical(g$target_line, "REV")) own else run_one(name, "REV")
  rows[[length(rows) + 1]] <- data.frame(
    serie = name, ligne = g$target_line,
    r12 = round(g$evidence_r, 3), crit12 = round(g$evidence_r_crit, 3), lag12 = g$causal_lag,
    n_line = own$n %||% NA, r_line = own$r %||% NA, crit_line = own$crit %||% NA,
    lag_line = own$lag %||% NA, part_line = own$partial %||% NA, rank_line = own$rank %||% NA,
    r_rev = rev$r %||% NA, rank_rev = rev$rank %||% NA,
    r_oi = round(g$bottom_r, 3), rank_oi = round(g$bottom_rank, 4), lag_oi = g$bottom_lag,
    stringsAsFactors = FALSE
  )
}
out <- do.call(rbind, rows)
options(width = 220)
print(out, row.names = FALSE)
cat("closed months", length(closed), closed[[1]], "to", closed[[length(closed)]], "\n")
cat("joint bar", round(0.05 / nrow(out), 4), "for", nrow(out), "series\n")
