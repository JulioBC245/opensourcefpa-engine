# SPDX-License-Identifier: AGPL-3.0-or-later
# Copyright (C) 2026 Julio Bauer Cantuaria
# Gate 3 — Monthly P&L reconciles to annual within tolerance; show per-line gap
source("tests/helpers.R", encoding = "UTF-8")
root <- fpa_test_bootstrap()
cfg_path <- fpa_ensure_fixtures(root)

res <- fpa_run(cfg_path, write = FALSE, stop_on_error = TRUE)
ann <- fpa_pnl_annual(res$pnl)
stopifnot(!is.null(ann), "2024" %in% ann$year)

# Build annual from raw import (sum of monthly mapped REV etc.)
raw <- res$reads$pnl_actuals$mapped
raw$year <- format(as.Date(raw$period_end), "%Y")
raw24 <- raw[raw$year == "2024", ]
annual_rev <- sum(raw24$montant[raw24$functional_label == "REV"])
engine_rev <- ann$REV[ann$year == "2024"]
# Engine REV = imported (+ optional volume overlay). Volumes off in fixture mechanisms.
gap <- engine_rev - annual_rev
# Fixed opex overlay does not touch REV
stopifnot(abs(gap) < 1)

# Per-line gaps table
ids <- c("REV", "COGS", "PAYROLL", "D_A", "FIN")
gaps <- vapply(ids, function(id) {
  a <- sum(raw24$montant[raw24$functional_label == id])
  e <- ann[[id]][ann$year == "2024"]
  e - a
}, numeric(1))
# OPEX_OTHER will gap by 12 * 4500 (loyer) — that is expected mechanical add-on
opex_gap <- ann$OPEX_OTHER[ann$year == "2024"] -
  sum(raw24$montant[raw24$functional_label == "OPEX_OTHER"])
stopifnot(abs(opex_gap - 12 * 4500) < 1)
stopifnot(all(abs(gaps) < 1))

message("PASS gate 3")
message(sprintf("  OPEX_OTHER annual gap (loyer): %.0f", opex_gap))
