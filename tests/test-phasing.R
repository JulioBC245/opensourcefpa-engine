# SPDX-License-Identifier: AGPL-3.0-or-later
# Copyright (C) 2026 Julio Bauer Cantuaria
# Budget phasing: seasonal profile, twelfths fallback, annual total preserved.
# LAST CHANGE: 2026-09-21
source("tests/helpers.R", encoding = "UTF-8")
root <- fpa_test_bootstrap()

# --- fiscal year months ---
ms <- fpa_fy_month_ends(as.Date("2025-12-31"))
stopifnot(length(ms) == 12L)
stopifnot(identical(ms[[1]], as.Date("2025-01-31")))
stopifnot(identical(ms[[12]], as.Date("2025-12-31")))
# Non-calendar year end still yields twelve consecutive months.
ms_june <- fpa_fy_month_ends(as.Date("2025-06-30"))
stopifnot(identical(ms_june[[1]], as.Date("2024-07-31")))
stopifnot(identical(ms_june[[12]], as.Date("2025-06-30")))

# --- seasonal shares from three complete years ---
profile <- c(5, 5, 8, 9, 10, 12, 14, 4, 9, 8, 8, 8) / 100
rows <- list()
for (y in 2022:2024) {
  for (m in 1:12) {
    rows[[length(rows) + 1]] <- data.frame(
      functional_label = "REV",
      period_end = as.character(fpa_month_end(y, m)),
      grain = "month",
      montant = 1200000 * profile[[m]],
      stringsAsFactors = FALSE
    )
  }
}
monthly <- do.call(rbind, rows)

shares <- fpa_seasonal_shares(monthly, "REV")
stopifnot(!is.null(shares))
stopifnot(length(shares) == 12L)
stopifnot(abs(sum(shares) - 1) < 1e-12)
# July is the peak in the fixture, August the trough.
stopifnot(which.max(shares) == 7L)
stopifnot(which.min(shares) == 8L)
stopifnot(abs(shares[[7]] - 0.14) < 1e-9)

# Thin history gives nothing rather than a bad profile.
stopifnot(is.null(fpa_seasonal_shares(monthly[1:12, , drop = FALSE], "REV")))
stopifnot(is.null(fpa_seasonal_shares(monthly, "PAYROLL")))

# --- shares applied preserve the voted total ---
amounts <- fpa_apply_shares(1000000, shares)
stopifnot(length(amounts) == 12L)
stopifnot(abs(sum(amounts) - 1000000) < 1e-9)

# An awkward total still reconciles to the cent.
odd <- fpa_apply_shares(100000.01, fpa_even_shares())
stopifnot(abs(sum(odd) - 100000.01) < 1e-9)

# --- phase an annual budget ---
budget_mapped <- data.frame(
  functional_label = c("REV", "COGS", "OPEX_OTHER"),
  period_end = "2025-12-31",
  grain = "year",
  montant = c(1000000, 400000, 240000),
  stringsAsFactors = FALSE
)
config <- list(
  budget_phasing = list(
    budget_shape = "annual",
    rows = list(
      list(line_id = "REV", method = "seasonal_index", follow = NULL, evidenced = TRUE),
      list(line_id = "COGS", method = "follow_line", follow = "REV", evidenced = TRUE),
      list(line_id = "OPEX_OTHER", method = "even", follow = NULL, evidenced = TRUE)
    )
  )
)

phased <- fpa_phase_budget(budget_mapped, monthly, config)
stopifnot(nrow(phased) == 36L)
stopifnot(all(c("periode", "functional_label", "montant", "method", "evidenced") %in% names(phased)))

rev_rows <- phased[phased$functional_label == "REV", , drop = FALSE]
stopifnot(nrow(rev_rows) == 12L)
stopifnot(abs(sum(rev_rows$montant) - 1000000) < 1e-9)
stopifnot(identical(unique(rev_rows$method), "seasonal_index"))
stopifnot(identical(rev_rows$periode[[1]], "2025-01"))
stopifnot(identical(rev_rows$periode[[12]], "2025-12"))
# July carries the peak share of the annual budget.
stopifnot(abs(rev_rows$montant[[7]] - 140000) < 0.01)

# COGS follows revenue, so the gross margin rate holds every month.
cogs_rows <- phased[phased$functional_label == "COGS", , drop = FALSE]
stopifnot(abs(sum(cogs_rows$montant) - 400000) < 1e-9)
ratio <- cogs_rows$montant / rev_rows$montant
stopifnot(max(abs(ratio - 0.4)) < 1e-6)

# A line with no evidence is spread evenly and says so.
opex_rows <- phased[phased$functional_label == "OPEX_OTHER", , drop = FALSE]
stopifnot(identical(unique(opex_rows$method), "even"))
stopifnot(abs(sum(opex_rows$montant) - 240000) < 1e-9)
stopifnot(max(abs(opex_rows$montant - 20000)) < 0.01)

# --- a seasonal request without evidence degrades to twelfths, never invents ---
config_thin <- list(
  budget_phasing = list(
    budget_shape = "annual",
    rows = list(
      list(line_id = "PAYROLL", method = "seasonal_index", follow = NULL, evidenced = TRUE)
    )
  )
)
thin <- fpa_phase_budget(
  data.frame(
    functional_label = "PAYROLL", period_end = "2025-12-31",
    grain = "year", montant = 600000, stringsAsFactors = FALSE
  ),
  monthly,
  config_thin
)
stopifnot(identical(unique(thin$method), "even"))
stopifnot(abs(sum(thin$montant) - 600000) < 1e-9)

# --- a monthly budget passes straight through ---
reads <- list(
  budget = list(
    ok = TRUE,
    grain = "month",
    mapped = data.frame(
      functional_label = "REV",
      period_end = as.character(fpa_month_end(2025, 1:3)),
      grain = "month",
      montant = c(10, 20, 30),
      stringsAsFactors = FALSE
    )
  )
)
straight <- fpa_budget_monthly(reads, list())
stopifnot(identical(unique(straight$method), "as_given"))
stopifnot(nrow(straight) == 3L)
stopifnot(identical(straight$periode[[1]], "2025-01"))

message("test-phasing ok")
