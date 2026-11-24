# SPDX-License-Identifier: AGPL-3.0-or-later
# Copyright (C) 2026 Julio Bauer Cantuaria
# Working capital uses the days in the month.
# Run from fpa-local: Rscript tests/check-wc-month.R

source("tests/helpers.R", encoding = "UTF-8")
fpa_test_bootstrap()

pass <- 0L
fail <- 0L
check <- function(label, ok) {
  if (isTRUE(ok)) { pass <<- pass + 1L; cat("PASS", label, "\n") } else { fail <<- fail + 1L; cat("FAIL", label, "\n") }
}

check("January has 31 days", identical(fpa_wc_month_days("2026-01"), 31L))
check("February 2026 has 28 days", identical(fpa_wc_month_days("2026-02"), 28L))
check("February 2024 has 29 days", identical(fpa_wc_month_days("2024-02"), 29L))
check("April has 30 days", identical(fpa_wc_month_days("2026-04"), 30L))

pnl_of <- function(periods, rev, cogs = 0, opex = 0, pay = 0) {
  data.frame(
    periode = periods, REV = rev, COGS = cogs, OPEX_OTHER = opex, PAYROLL = pay,
    NI = 0, D_A = 0, stringsAsFactors = FALSE
  )
}
assum_of <- function(periods, dso, dpo, dio) {
  data.frame(periode = periods, dso = dso, dpo = dpo, dio = dio, stringsAsFactors = FALSE)
}
cfg <- list(mechanisms = list(dso_dpo_dio = TRUE))

p30 <- "2026-04"
wc <- fpa_build_wc(pnl_of(p30, 30000), assum_of(p30, 30, 0, 0), cfg)
check("30 000 at 30 days in a 30-day month is 30 000", abs(wc$AR[[p30]] - 30000) < 0.01)

p28 <- "2026-02"
wc28 <- fpa_build_wc(pnl_of(p28, 28000), assum_of(p28, 28, 0, 0), cfg)
check("February: 28 days of credit on 28 000 is 28 000", abs(wc28$AR[[p28]] - 28000) < 0.01)

p31 <- "2026-01"
wc31 <- fpa_build_wc(pnl_of(p31, 31000), assum_of(p31, 31, 0, 0), cfg)
check("a 31-day month at 31 days is the month's sales", abs(wc31$AR[[p31]] - 31000) < 0.01)

peak <- c("2026-01", "2026-08")
wcp <- fpa_build_wc(pnl_of(peak, c(10000, 40000)), assum_of(peak, 30, 0, 0), cfg)
check("a seasonal peak follows the month", wcp$AR[["2026-08"]] > wcp$AR[["2026-01"]] * 3)

z <- fpa_build_wc(pnl_of(p30, 30000), assum_of(p30, 0, 0, 0), cfg)
check("zero days give zero", abs(z$AR[[p30]]) < 0.01)

year <- c("2025-12", "2026-01")
wcy <- fpa_build_wc(
  pnl_of(year, c(31000, 31000), cogs = c(1000, 1000)),
  assum_of(year, 0, 0, 0),
  cfg,
  opening = c(AR = 0, AP = 0, INV = 0, OTHER_CL = 0, CASH = 0)
)
impact <- fpa_wc_cash_impact(wcy, year, opening = c(AR = 0, AP = 0, INV = 0, OTHER_CL = 0))
check("January after December: the cash change is the stock change",
  abs(impact[["2026-01"]] - (-(wcy$AR[["2026-01"]] - wcy$AR[["2025-12"]]))) < 0.01)

flat <- c("2026-01", "2026-02")
wcf <- fpa_build_wc(pnl_of(flat, c(31000, 28000)), assum_of(flat, c(31, 28), 0, 0), cfg,
                    opening = c(AR = 31000, AP = 0, INV = 0, OTHER_CL = 0))
rel <- fpa_wc_opening_release(wcf, flat, c(AR = 31000, AP = 0, INV = 0, OTHER_CL = 0))
check("a matching opening releases nothing", abs(rel[["2026-01"]]) < 0.01 && abs(rel[["2026-02"]]) < 0.01)

high <- fpa_wc_opening_release(wcf, flat, c(AR = 20000, AP = 0, INV = 0, OTHER_CL = 0))
check("a high opening releases the gap", abs(high[["2026-01"]] - (20000 - wcf$AR[["2026-01"]])) < 0.01)

pnl <- pnl_of(flat, c(31000, 28000), pay = c(3000, 3000))
pnl$NI <- c(100, 100)
wcbs <- fpa_build_wc(pnl, assum_of(flat, 12, 0, 0), cfg, opening = c(AR = 5000, AP = 0, INV = 0, OTHER_CL = 0, CASH = 1000))
tot <- fpa_wc_cash_impact(wcbs, flat, opening = c(AR = 5000, AP = 0, INV = 0, OTHER_CL = 0, CASH = 1000))
rel2 <- fpa_wc_opening_release(wcbs, flat, c(AR = 5000, AP = 0, INV = 0, OTHER_CL = 0, CASH = 1000))
cash <- fpa_build_cash_bridge(pnl, tot - rel2, setNames(c(0, 0), flat), setNames(c(0, 0), flat), 1000, rel2)
check("the bridge still ties", abs(cash$cash_close[[2]] - (1000 + sum(cash$delta_cash))) < 0.01)
check("cash change equals the working-capital identity",
  abs(sum(cash$wc_impact + cash$wc_opening_release) - sum(tot)) < 0.01)

cat(sprintf("PASS %d FAIL %d\n", pass, fail))
quit(status = if (fail) 1L else 0L)
