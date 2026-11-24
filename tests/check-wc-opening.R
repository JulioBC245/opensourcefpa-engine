# SPDX-License-Identifier: AGPL-3.0-or-later
# Copyright (C) 2026 Julio Bauer Cantuaria
# Opening working capital is its own bridge step.
# Run from fpa-local: Rscript tests/check-wc-opening.R

source("tests/helpers.R", encoding = "UTF-8")
fpa_test_bootstrap()

pass <- 0L
fail <- 0L
check <- function(label, ok) {
  if (isTRUE(ok)) { pass <<- pass + 1L; cat("PASS", label, "\n") } else { fail <<- fail + 1L; cat("FAIL", label, "\n") }
}

periods <- c("2026-01", "2026-02")
pnl <- data.frame(
  periode = periods, REV = c(2756, 2800), COGS = c(800, 800), OPEX_OTHER = c(200, 200),
  PAYROLL = c(1000, 1000), NI = c(-100, -100), D_A = c(50, 50),
  stringsAsFactors = FALSE
)
assum <- data.frame(periode = periods, dso = 12, dpo = 35, dio = 8, stringsAsFactors = FALSE)
cfg <- list(mechanisms = list(dso_dpo_dio = TRUE))
opening <- c(AR = 20000, AP = 4000, INV = 1000, OTHER_CL = 500, CASH = 10000)
wc <- fpa_build_wc(pnl, assum, cfg, opening)
rel <- fpa_wc_opening_release(wc, periods, opening)
tot <- fpa_wc_cash_impact(wc, periods, opening)
cash <- fpa_build_cash_bridge(pnl, tot - rel, setNames(c(0, 0), periods), setNames(c(0, 0), periods), 10000, rel)

check("the release sits on the first month only", abs(rel[["2026-02"]]) < 0.01 && abs(rel[["2026-01"]]) > 1)
check("BFR excludes the opening release", abs(cash$wc_impact[[1]] + cash$wc_opening_release[[1]] - tot[[1]]) < 0.01)
check("the bridge ties from opening to closing cash",
  abs(cash$cash_open[[1]] - 10000) < 0.01 &&
  abs(cash$cash_close[[2]] - (10000 + sum(cash$delta_cash))) < 0.01)

flags <- fpa_wc_opening_flags(wc, periods, opening, assum)
check("a large receivables gap is flagged", any(grepl("créances clients", flags)) && any(grepl("20 000", flags)))
matched <- opening
matched[["AR"]] <- wc$AR[["2026-01"]]
matched[["AP"]] <- wc$AP[["2026-01"]]
matched[["INV"]] <- wc$INV[["2026-01"]]
matched[["OTHER_CL"]] <- wc$OTHER_CL[["2026-01"]]
check("a matching opening is not flagged", !length(fpa_wc_opening_flags(wc, periods, matched, assum)))

cat(sprintf("PASS %d FAIL %d\n", pass, fail))
quit(status = if (fail) 1L else 0L)
