# SPDX-License-Identifier: AGPL-3.0-or-later
# Copyright (C) 2026 Julio Bauer Cantuaria
# Summary: realized totals exclude forecast months, and a hover names the month.
source("tests/helpers.R", encoding = "UTF-8")
fpa_test_bootstrap()

pnl <- data.frame(
  periode = c("2024-01", "2024-02", "2025-01", "2025-02"),
  REV = c(100, 150, 80, 90),
  OI = c(20, 30, 10, 40),
  stringsAsFactors = FALSE
)
status <- data.frame(
  periode = pnl$periode,
  status = c("actual", "actual", "actual", "forecast"),
  stringsAsFactors = FALSE
)
variance <- data.frame(
  periode = "2025-01", line_id = "REV", budget = 70, variance = 10,
  stringsAsFactors = FALSE
)
cash <- data.frame(
  periode = pnl$periode,
  cash_open = c(10, 5, 0, -2),
  cash_close = c(5, 0, -2, 4),
  delta_cash = c(-5, -5, -2, 6),
  stringsAsFactors = FALSE
)

b <- fpa_synthese_brief(pnl, cash, status, variance)
stopifnot(b$n_actual == 3L)
stopifnot(b$n_forecast == 1L)
stopifnot(b$rev_actual == 330)
stopifnot(b$rev_forecast == 90)
stopifnot(identical(b$first_forecast, "2025-02"))
stopifnot(identical(b$peak_month, "2024-02"))
stopifnot(b$cash_under == 1L)
stopifnot(identical(b$cash_low_month, "2025-01"))
stopifnot(grepl("prévision commence en 2025-02", b$trend))
stopifnot(grepl("sous zéro", b$cash_note))

hover <- fpa_synthese_hover_pnl(b$frame)
stopifnot(length(hover) == 4L)
stopifnot(grepl("2025-02 · prévision", hover[[4]], fixed = TRUE))
stopifnot(grepl("2025-01 · réalisé", hover[[3]], fixed = TRUE))
stopifnot(grepl("Budget 70 EUR", hover[[3]], fixed = TRUE))
stopifnot(grepl("Même mois l'an dernier", hover[[3]], fixed = TRUE))
stopifnot(!grepl("Budget", hover[[1]], fixed = TRUE))

message("PASS synthese")
