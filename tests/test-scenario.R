# SPDX-License-Identifier: AGPL-3.0-or-later
# Copyright (C) 2026 Julio Bauer Cantuaria
# Scenario levers: days stay days, a rate takes points, everything else takes percent.
source("tests/helpers.R", encoding = "UTF-8")
fpa_test_bootstrap()

stopifnot(identical(fpa_scenario_lever_spec("dso")$kind, "days"))
stopifnot(identical(fpa_scenario_lever_spec("dpo")$kind, "days"))
stopifnot(identical(fpa_scenario_lever_spec("taux.impot")$kind, "points"))
stopifnot(identical(fpa_scenario_lever_spec("charge.fixe.loyer")$kind, "percent"))
stopifnot(fpa_scenario_lever_spec("dso")$step == 7)

# +7 days on a 45-day DSO is 52 days, not 45 times anything.
stopifnot(fpa_scenario_value(fpa_scenario_lever_spec("dso"), 45, 7) == 52)
stopifnot(fpa_scenario_value(fpa_scenario_lever_spec("dso"), 45, -21) == 24)
# A rate held as a fraction moves in hundredths.
stopifnot(abs(fpa_scenario_value(fpa_scenario_lever_spec("taux.impot"), 0.25, 3) - 0.28) < 1e-12)
# The same rate written as 25 moves in whole points.
stopifnot(abs(fpa_scenario_value(fpa_scenario_lever_spec("taux.impot"), 25, 3) - 28) < 1e-12)
# Percent levers scale.
stopifnot(abs(fpa_scenario_value(fpa_scenario_lever_spec("charge.fixe.loyer"), 1000, -30) - 700) < 1e-9)

wide <- data.frame(periode = c("2026-01", "2026-02"), dso = c(40, 45), taux.impot = c(0.25, 0.25))
base <- fpa_scenario_base_values(wide, c("dso", "taux.impot"))
stopifnot(base$dso == 45)

ov <- fpa_scenario_overrides(base, list(dso = 7, taux.impot = 0))
stopifnot(identical(names(ov), "dso"))
stopifnot(ov$dso == 52)
stopifnot(length(fpa_scenario_overrides(base, list(dso = 0))) == 0L)

ls <- fpa_scenario_line_scale(list(REV = -10, PAYROLL = 0))
stopifnot(identical(names(ls), "REV"))
stopifnot(abs(ls$REV - 0.9) < 1e-12)

moves <- fpa_scenario_moves(base, list(dso = 7), list(REV = -10))
stopifnot(any(grepl("Encaissement clients \\+7 jours", moves)))
stopifnot(any(grepl("REV -10 %", moves)))
stopifnot(length(fpa_scenario_moves(base, list(dso = 0), list())) == 0L)

# A declared lever on the assumption_keys row beats the id guess.
declared <- list(id = "dso", lever = "percent", min = -10, max = 10, step = 5, label = "Delai declare")
stopifnot(identical(fpa_scenario_lever_spec("dso", declared)$kind, "percent"))
stopifnot(fpa_scenario_lever_spec("dso", declared)$min == -10)
stopifnot(identical(fpa_scenario_label("dso", declared), "Delai declare"))
rate <- list(id = "cout.variable.pct", lever = "points", label = "Cout variable")
stopifnot(abs(fpa_scenario_value(fpa_scenario_lever_spec("cout.variable.pct", rate), 0.18, 2) - 0.20) < 1e-12)
ov_rate <- fpa_scenario_overrides(
  list(cout.variable.pct = 0.18),
  list(cout.variable.pct = 2),
  list(rate)
)
stopifnot(abs(ov_rate$cout.variable.pct - 0.20) < 1e-12)
moves_declared <- fpa_scenario_moves(list(dso = 45), list(dso = -10), list(), list(declared))
stopifnot(any(grepl("Delai declare -10 %", moves_declared)))

# Missing scenario_lines keeps the four lines. An empty array asks for none.
kept <- fpa_scenario_lines(list(), c("REV", "COGS", "TAX"))
stopifnot(identical(kept$ids, c("REV", "COGS")))
none <- fpa_scenario_lines(list(scenario_lines = list()), c("REV", "COGS"))
stopifnot(length(none$ids) == 0L)
custom <- fpa_scenario_lines(
  list(scenario_lines = list(list(id = "TAX", min = -3, max = 3, step = 1), list(id = "MISSING"))),
  c("REV", "TAX")
)
stopifnot(identical(custom$ids, "TAX"))
stopifnot(custom$spec$TAX$min == -3)

cash <- data.frame(
  periode = c("2026-01", "2026-02", "2026-03"),
  cash_open = c(1000, 400, -200),
  cash_close = c(400, -200, 900)
)
kpi <- fpa_scenario_cash_kpi(list(cash = cash, pnl = data.frame(OI = c(1, 2, 3))), floor = 0)
stopifnot(kpi$open == 1000)
stopifnot(kpi$close == 900)
stopifnot(kpi$low == -200)
stopifnot(identical(kpi$low_month, "2026-02"))
stopifnot(kpi$under == 1L)
stopifnot(kpi$oi == 6)
stopifnot(kpi$oi_fy == 6, kpi$oi_last == 3)
kpi_ye <- fpa_scenario_cash_kpi(
  list(cash = cash, pnl = data.frame(periode = as.character(cash$periode), OI = seq_len(nrow(cash)))),
  floor = 0, ye_month = as.character(cash$periode[[nrow(cash)]])
)
stopifnot(kpi_ye$oi_fy == sum(seq_len(nrow(cash))), kpi_ye$oi_last == nrow(cash))
stopifnot(fpa_scenario_cash_kpi(list(cash = cash, pnl = data.frame(OI = 1)), floor = 500)$under == 2L)
stopifnot(is.na(fpa_scenario_cash_kpi(NULL)$close))

message("PASS scenario")
