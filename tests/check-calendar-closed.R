# SPDX-License-Identifier: AGPL-3.0-or-later
# Copyright (C) 2026 Julio Bauer Cantuaria
# Closed-month capex and debt still move reconstructed cash.
# Depreciation starts in the first open month. The closed P&L is unchanged.
# Run from the opensourcefpa-engine root: Rscript tests/check-calendar-closed.R

source("tests/helpers.R", encoding = "UTF-8")
fpa_test_bootstrap()

pass <- 0L
fail <- 0L
check <- function(label, ok) {
  if (isTRUE(ok)) { pass <<- pass + 1L; cat("PASS", label, "\n") } else { fail <<- fail + 1L; cat("FAIL", label, "\n") }
}

periods <- c("2026-02", "2026-03", "2026-10")
cal <- data.frame(
  date = as.Date(c("2026-02-15", "2026-02-20", "2026-11-01", "2026-03-01")),
  libelle = c("Lit", "Emprunt", "Hors", "Inconnu"),
  montant = c(9000, 15000, 1000, 500),
  type = c("capex", "dette_tirage", "capex", "cadeau"),
  periode = c("2026-02", "2026-02", "2026-11", "2026-03"),
  life_months = c(10, NA, 10, NA),
  interest_rate = NA_real_,
  break_cost = 0,
  stringsAsFactors = FALSE
)
led <- fpa_decisions_ledger(cal, periods)
check("February capex is used", led$state[led$libelle == "Lit"] == "used")
check("February draw is used", led$state[led$libelle == "Emprunt"] == "used")
check("a month outside the horizon is excluded", led$reason[led$libelle == "Hors"] == "hors horizon")
check("an unknown type is excluded", led$reason[led$libelle == "Inconnu"] == "type inconnu")

dated <- fpa_decisions_dated(cal, periods)
open <- fpa_decisions_open(cal, c("2026-02", "2026-03"))
check("the dock still hides closed months", !any(open$periode == "2026-02"))
check("cash keeps the closed-month events", any(dated$libelle == "Lit") && any(dated$libelle == "Emprunt"))

capex <- fpa_capex_by_month(dated, periods)
debt <- fpa_debt_cash_by_month(dated, periods)
check("February cash pays the capex and receives the draw",
  abs(capex[["2026-02"]] - 9000) < 0.01 && abs(debt$net[["2026-02"]] - 15000) < 0.01)

da <- fpa_da_from_capex(dated, periods, not_before = "2026-10")
check("depreciation starts in the first open month",
  abs(da[["2026-02"]]) < 0.01 && abs(da[["2026-03"]]) < 0.01 && abs(da[["2026-10"]] - 900) < 0.01)

pnl <- data.frame(
  periode = periods, REV = 1000, COGS = 200, OPEX_OTHER = 100, PAYROLL = 300,
  NI = c(50, 50, 50), D_A = c(10, 10, 10), stringsAsFactors = FALSE
)
pnl_before <- pnl$NI
cfg <- list(mechanisms = list(dso_dpo_dio = TRUE))
assum <- data.frame(periode = periods, dso = 0, dpo = 0, dio = 0, stringsAsFactors = FALSE)
wc <- fpa_build_wc(pnl, assum, cfg)
opening <- c(CASH = 20000, AR = 0, AP = 0, INV = 0, OTHER_CL = 0, PPE = 40000, EQUITY = 60000, ST_DEBT = 0, LT_DEBT = 0, OTHER_CA = 0)
impact <- fpa_wc_cash_impact(wc, periods, opening)
cash <- fpa_build_cash_bridge(pnl, impact, capex, debt$net, 20000)
bs <- fpa_build_bs(pnl, wc, cash, capex, debt, opening)
ident <- fpa_assert_identity(bs)
check("February cash is lower by the capex and higher by the draw",
  abs(cash$cash_close[[1]] - (20000 + pnl$NI[[1]] + pnl$D_A[[1]] + impact[[1]] - 9000 + 15000)) < 0.05)
check("February fixed assets rise by the capex", abs(bs$PPE[[1]] - (40000 + 9000 - pnl$D_A[[1]])) < 0.05)
check("the closed P&L is unchanged", identical(pnl$NI, pnl_before))
check("the balance sheet balances every month", isTRUE(ident$ok))

cat(sprintf("PASS %d FAIL %d\n", pass, fail))
quit(status = if (fail) 1L else 0L)
