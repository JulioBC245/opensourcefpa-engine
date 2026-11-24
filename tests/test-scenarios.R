# SPDX-License-Identifier: AGPL-3.0-or-later
# Copyright (C) 2026 Julio Bauer Cantuaria
# Scenarios: low is the unfavourable case. Actual months stay put.
source("tests/helpers.R", encoding = "UTF-8")
root <- fpa_test_bootstrap()

stopifnot(abs(fpa_scenario_input_factor("tarif", 10, "low") - 0.9) < 1e-9)
stopifnot(abs(fpa_scenario_input_factor("tarif", 10, "high") - 1.1) < 1e-9)
stopifnot(abs(fpa_scenario_input_factor("cout.variable.pct", 10, "low") - 1.1) < 1e-9)
stopifnot(abs(fpa_scenario_input_factor("cout.variable.pct", 10, "high") - 0.9) < 1e-9)
stopifnot(abs(fpa_scenario_input_factor("dpo", 10, "low") - 0.9) < 1e-9)
stopifnot(abs(fpa_scenario_input_factor("dso", 10, "low") - 1.1) < 1e-9)
stopifnot(abs(fpa_scenario_input_factor("tarif", 10, "base") - 1) < 1e-9)

pnl <- data.frame(
  periode = c("2026-08", "2026-09"),
  REV = c(100, 100),
  OTHER_OP_INC = c(0, 0),
  COGS = c(40, 40),
  PAYROLL = c(20, 20),
  OPEX_OTHER = c(10, 10),
  D_A = c(0, 0),
  FIN = c(0, 0),
  EXCEP = c(0, 0),
  TAX = c(0, 0),
  stringsAsFactors = FALSE
)
pnl <- fpa_pnl_derive(pnl)
inputs <- data.frame(
  id = c("tarif", "cout.variable.pct"),
  low_pct = c(10, 10),
  high_pct = c(10, 10),
  stringsAsFactors = FALSE
)
cfg <- list(mechanisms = list(variable_cost_pct_or_unit = TRUE), scenario_ranges = list(inputs = inputs))
low <- fpa_scenario_pnl(pnl, inputs, "low", "2026-08", cfg)
# August is actual: untouched. September: REV 90, COGS 40 * 0.9 * 1.1 = 39.6
stopifnot(abs(low$REV[[1]] - 100) < 1e-9)
stopifnot(abs(low$REV[[2]] - 90) < 1e-9)
stopifnot(abs(low$COGS[[2]] - 39.6) < 1e-6)
# OI base September = 30. Low = 90 - 39.6 - 20 - 10 = 20.4, which is worse.
stopifnot(low$OI[[2]] < pnl$OI[[2]])

high <- fpa_scenario_pnl(pnl, inputs, "high", "2026-08", cfg)
stopifnot(high$OI[[2]] > pnl$OI[[2]])

cash <- data.frame(periode = pnl$periode, cash_close = c(50, 80), stringsAsFactors = FALSE)
built <- fpa_scenarios_build(pnl, cash, cfg, "2026-08", NULL, NULL)
stopifnot(nrow(built) == 6L)
stopifnot(all(built$case == rep(c("low", "base", "high"), each = 2L)))
base <- built[built$case == "base", , drop = FALSE]
stopifnot(abs(base$OI[[2]] - pnl$OI[[2]]) < 1e-6)
stopifnot(abs(base$cash_close[[2]] - 80) < 1e-6)
low_row <- built[built$case == "low" & built$periode == "2026-09", , drop = FALSE]
stopifnot(low_row$cash_close[[1]] < 80)

budget <- data.frame(
  periode = c("2026-08", "2026-09"),
  functional_label = c("OI", "OI"),
  montant = c(25, 30),
  stringsAsFactors = FALSE
)
with_budget <- fpa_scenarios_build(pnl, cash, cfg, "2026-08", NULL, budget)
sep_low <- with_budget[with_budget$case == "low" & with_budget$periode == "2026-09", , drop = FALSE]
sep_high <- with_budget[with_budget$case == "high" & with_budget$periode == "2026-09", , drop = FALSE]
stopifnot(abs((sep_low$OI - sep_low$oi_vs_budget) - 30) < 1e-6)
stopifnot(abs((sep_high$OI - sep_high$oi_vs_budget) - 30) < 1e-6)
stopifnot(abs(sep_low$oi_vs_budget - (20.4 - 30)) < 1e-6)

ranks <- fpa_scenario_ranks(pnl, cfg, "2026-08")
stopifnot(nrow(ranks) == 2L)
stopifnot(ranks$id[[1]] == "tarif")
stopifnot(ranks$oi_span[[1]] >= ranks$oi_span[[2]])
stopifnot(ranks$oi_low[[1]] < ranks$oi_base[[1]])
stopifnot(ranks$oi_high[[1]] > ranks$oi_base[[1]])

status <- data.frame(periode = c("2026-08", "2026-09"), status = c("actual", "forecast"), stringsAsFactors = FALSE)
tot <- fpa_scenario_open_totals(with_budget, status)
stopifnot(abs(tot$oi_budget - 30) < 1e-6)
stopifnot(abs(tot$oi_base - pnl$OI[[2]]) < 1e-6)
sent <- fpa_scenario_sentence(with_budget, status)
stopifnot(grepl("30", sent))
stopifnot(grepl("budget", sent, ignore.case = TRUE))

# Three months of budget inside a longer open window must not invent a long budget total.
long_pnl <- data.frame(
  periode = c("2026-10", "2026-11", "2026-12", "2027-01", "2027-02"),
  REV = c(100, 100, 100, 100, 100),
  OTHER_OP_INC = 0, COGS = 40, PAYROLL = 20, OPEX_OTHER = 10,
  D_A = 0, FIN = 0, EXCEP = 0, TAX = 0,
  stringsAsFactors = FALSE
)
long_pnl <- fpa_pnl_derive(long_pnl)
long_status <- data.frame(
  periode = long_pnl$periode,
  status = "forecast",
  stringsAsFactors = FALSE
)
long_budget <- data.frame(
  periode = c("2026-10", "2026-11", "2026-12"),
  functional_label = "OI",
  montant = c(25, 25, 25),
  stringsAsFactors = FALSE
)
long_built <- fpa_scenarios_build(long_pnl, NULL, cfg, character(), NULL, long_budget)
long_tot <- fpa_scenario_open_totals(long_built, long_status)
stopifnot(identical(long_tot$n_open, 5L))
stopifnot(identical(long_tot$n_budget_missing, 2L))
stopifnot(!is.finite(long_tot$oi_budget))
stopifnot(is.finite(long_tot$oi_base))
long_sent <- fpa_scenario_sentence(long_built, long_status)
stopifnot(grepl("2", long_sent) && grepl("5", long_sent))
stopifnot(grepl("n'ont pas de budget|have no budget", long_sent, ignore.case = TRUE))
stopifnot(!is.finite(long_tot$oi_budget))

full_budget <- data.frame(
  periode = long_pnl$periode,
  functional_label = "OI",
  montant = c(25, 25, 25, 25, 25),
  stringsAsFactors = FALSE
)
full_built <- fpa_scenarios_build(long_pnl, NULL, cfg, character(), NULL, full_budget)
full_tot <- fpa_scenario_open_totals(full_built, long_status)
stopifnot(is.finite(full_tot$oi_budget))
stopifnot(identical(full_tot$n_budget_missing, 0L))
stopifnot(abs(full_tot$oi_budget - 125) < 1e-6)

stopifnot(identical(fpa_scn_budget_total(c(10, 20, NA)), NA_real_))
stopifnot(abs(fpa_scn_budget_total(c(10, 20, 30)) - 60) < 1e-9)

message("PASS scenarios")
