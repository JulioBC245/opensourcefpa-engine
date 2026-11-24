# SPDX-License-Identifier: AGPL-3.0-or-later
# Copyright (C) 2026 Julio Bauer Cantuaria
# Horizon, period labels, closed months, identity across the boundary.
source("tests/helpers.R", encoding = "UTF-8")
root <- fpa_test_bootstrap()

mapped <- data.frame(
  period_end = as.Date("2024-01-31"),
  functional_label = "REV",
  montant = 100,
  stringsAsFactors = FALSE
)
short <- fpa_build_pnl(mapped, periods = "2024-01")
long <- fpa_build_pnl(mapped, periods = c("2024-01", "2024-02"))
stopifnot(identical(short$REV[[1]], long$REV[[1]]))
stopifnot(abs(long$REV[[2]]) < 1e-9)

ids <- fpa_pnl_input_ids()
pnl <- data.frame(periode = c("2024-01", "2024-02"), stringsAsFactors = FALSE)
for (id in ids) pnl[[id]] <- c(0, 0)
pnl$REV <- c(100, 0)
pnl <- fpa_pnl_derive(pnl)
msgs <- fpa_forecast_refusals(pnl, "2024-01", "REV")
stopifnot(length(msgs) == 1L, grepl("mecanisme", msgs[[1]]), grepl("zero", msgs[[1]], ignore.case = TRUE))

config <- list(mechanisms = list(revenue_growth_pct = TRUE))
assum <- data.frame(periode = c("2024-01", "2024-02"), croissance.rev = c(0.1, 0.1))
filled <- fpa_pnl_apply_growth(pnl, assum, config, "2024-02")
stopifnot(abs(filled$REV[[2]] - 110) < 1e-6)
stopifnot(!length(fpa_forecast_refusals(filled, "2024-01", "REV")))

label_cell <- function(line, periode) {
  if (periode %in% "2024-02") return("mechanical_rule")
  if (identical(line, "REV")) return("imported_actual")
  "mechanical_rule"
}
labelled <- fpa_label_statement(filled, label_cell)
fwd <- labelled[labelled$periode == "2024-02", , drop = FALSE]
stopifnot(nrow(fwd) > 0, !any(fwd$evidence_label == "imported_actual"))

# Identity holds on a two-month frame, the second month being a forecast.
periods <- c("2024-01", "2024-02")
opening <- c(CASH = 50, AR = 0, INV = 0, AP = 0, PPE = 20, ST_DEBT = 0, LT_DEBT = 0,
             OTHER_CA = 0, OTHER_CL = 0, EQUITY = 70)
wc_cfg <- list(mechanisms = list(dso_dpo_dio = FALSE))
wc <- fpa_build_wc(filled, NULL, wc_cfg, opening)
wc_impact <- fpa_wc_cash_impact(wc, periods, opening)
zero <- stats::setNames(c(0, 0), periods)
debt <- list(draw = zero, repay = zero, break_cost = zero, net = zero)
cash <- fpa_build_cash_bridge(filled, wc_impact, zero, zero, opening[["CASH"]])
bs <- fpa_build_bs(filled, wc, cash, zero, debt, opening)
ident <- fpa_assert_identity(bs)
stopifnot(isTRUE(ident$ok), nrow(bs) == 2L)

message("PASS horizon")
