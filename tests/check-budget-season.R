# SPDX-License-Identifier: AGPL-3.0-or-later
# Copyright (C) 2026 Julio Bauer Cantuaria
# Seasonal budget: a flat voted year follows the kept prior years. The total does not move.
# Run from the opensourcefpa-engine root: Rscript tests/check-budget-season.R
source("tests/helpers.R", encoding = "UTF-8")
fpa_test_bootstrap()

pass <- 0L
fail <- 0L
check <- function(label, ok) {
  if (isTRUE(ok)) { pass <<- pass + 1L; cat("PASS", label, "\n") } else { fail <<- fail + 1L; cat("FAIL", label, "\n") }
}
near <- function(a, b, tol = 1) is.finite(a) && is.finite(b) && abs(a - b) <= tol

profile <- c(5, 5, 8, 9, 10, 12, 14, 4, 9, 8, 8, 8) / 100
rows <- list()
for (y in 2022:2024) {
  amp <- if (y == 2023L) { z <- profile; z[[8]] <- profile[[8]] * 4; z } else profile
  for (m in 1:12) {
    rows[[length(rows) + 1L]] <- data.frame(
      functional_label = "REV",
      period_end = as.character(fpa_month_end(y, m)),
      montant = 100000 * amp[[m]],
      stringsAsFactors = FALSE
    )
  }
}
monthly <- do.call(rbind, rows)
budget <- data.frame(
  periode = sprintf("2025-%02d", 1:12),
  functional_label = "REV",
  montant = rep(1000, 12),
  method = "as_given",
  stringsAsFactors = FALSE
)
on <- list(budget_season = list(shape_from_history = TRUE, drop_odd_months = TRUE, lookback_years = 3L))

check("a missing block leaves the flat budget untouched", {
  same <- fpa_budget_reshape(budget, monthly, list())
  all(same$montant == 1000) && !isTRUE(attr(same, "season")$on)
})

shaped <- fpa_budget_reshape(budget, monthly, on)
info <- attr(shaped, "season")
check("three years are kept and the switch is on", isTRUE(info$on) && identical(sort(info$years), c("2022", "2023", "2024")))
check(sprintf("voted total stays 12 000 (got %s)", sum(shaped$montant)), near(sum(shaped$montant), 12000, 0.01))
check("August 2023 is set aside", "2023-08" %in% info$dropped)
check("August is no longer the flat 1 000", abs(shaped$montant[8] - 1000) > 50)
check("July is the high month, August the low one", shaped$montant[7] > shaped$montant[1] && shaped$montant[7] > shaped$montant[8])

one <- fpa_budget_reshape(budget, monthly, on, keep_years = "2024")
check("one year keeps that year's shape", one$montant[7] > one$montant[8] && abs(one$montant[7] - 1000) > 50)
check("one year still totals 12 000", near(sum(one$montant), 12000, 0.01))
check("one year does not fall back to flat", !any(attr(one, "season")$fallback %in% c("none", "need_two")))
none <- fpa_budget_reshape(budget, monthly, on, keep_years = character(0))
check("no year ticked: the budget stays flat", all(abs(none$montant - 1000) < 0.01) && "none" %in% attr(none, "season")$fallback)

voted <- budget
voted$montant <- c(400, 500, 800, 900, 1000, 1200, 1600, 700, 900, 800, 700, 500)
check("a budget that already has a shape is not overwritten", {
  kept <- fpa_budget_reshape(voted, monthly, on)
  all(abs(kept$montant - voted$montant) < 0.01)
})

without <- fpa_budget_reshape(budget, monthly, on, keep_years = c("2022", "2024"))
check("asking for two years uses those two years", identical(sort(attr(without, "season")$years), c("2022", "2024")))

var <- data.frame(
  periode = budget$periode, line_id = "REV", actual = rep(1500, 12), budget = rep(1000, 12),
  variance = rep(500, 12), variance_pct = rep(0.5, 12),
  price = rep(10, 12), volume = rep(20, 12), mix = rep(0, 12),
  rate = NA_real_, headcount = NA_real_, unexplained = rep(470, 12),
  stringsAsFactors = FALSE
)
reb <- fpa_variance_rebudget(var, shaped)
check("named pieces stay, the gap absorbs the new budget", all(reb$price == 10) && all(abs((reb$variance) - (1500 - reb$budget)) < 0.01))
check("price + volume + mix + unexplained still equals the gap",
      all(abs(reb$price + reb$volume + reb$mix + reb$unexplained - reb$variance) < 0.01))
check("the year budget in the variance is still 12 000", near(sum(reb$budget), 12000, 0.01))

hotel <- fpa_optional_data_config()
if (!is.null(hotel)) {
  cfg <- fpa_load_config(hotel)
  paths <- fpa_paths(cfg)
  bud <- utils::read.csv(file.path(paths$work, "budget-monthly.csv"), stringsAsFactors = FALSE)
  mapped <- fpa_season_mapped(cfg, paths)
  rev <- bud[bud$functional_label == "REV", , drop = FALSE]
  live <- fpa_budget_reshape(bud, mapped, cfg)
  live_rev <- live[live$functional_label == "REV", , drop = FALSE]
  check("hotel: the switch is on", isTRUE(fpa_budget_season_on(cfg)))
  check(sprintf("hotel: revenue total stays %s", round(sum(rev$montant))), near(sum(live_rev$montant), sum(rev$montant), 0.05))
  check("hotel: January and August of the budget now differ", abs(live_rev$montant[live_rev$periode == "2026-01"] - live_rev$montant[live_rev$periode == "2026-08"]) > 500)
  cat("hotel years:", paste(attr(live, "season")$years, collapse = ", "), "\n")
  cat("hotel months set aside:", paste(attr(live, "season")$dropped, collapse = ", "), "\n")
  one_h <- fpa_budget_reshape(bud, mapped, cfg, keep_years = "2025")
  one_rev <- one_h[one_h$functional_label == "REV", , drop = FALSE]
  check("hotel, 2025 only: the voted total stays", near(sum(one_rev$montant), sum(rev$montant), 0.05))
  check("hotel, 2025 only: January and August differ", abs(one_rev$montant[one_rev$periode == "2026-01"] - one_rev$montant[one_rev$periode == "2026-08"]) > 500)
  cat("hotel 2025 only jan/aug:",
      one_rev$montant[one_rev$periode == "2026-01"],
      one_rev$montant[one_rev$periode == "2026-08"], "\n")
} else {
  fpa_notify_skip_optional_data("hotel budget season checks")
}

cat(sprintf("\n%d pass, %d fail\n", pass, fail))
if (fail > 0L) quit(status = 1L)
