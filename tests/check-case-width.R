# SPDX-License-Identifier: AGPL-3.0-or-later
# Copyright (C) 2026 Julio Bauer Cantuaria
# A forecast month keeps its scenario percent even when the driver form is present.
# Run from fpa-local: Rscript tests/check-case-width.R

source("tests/helpers.R", encoding = "UTF-8")
fpa_test_bootstrap()

src <- readLines("app.R", encoding = "UTF-8", warn = FALSE)
start <- grep("^fpa_shiny_shock_number <- function", src)[[1]]
end <- grep("^fpa_shiny_stitch_pnl <- function", src)[[1]]
eval(parse(text = src[start:(end - 1L)]))

pass <- 0L
fail <- 0L
check <- function(label, ok) {
  if (isTRUE(ok)) {
    pass <<- pass + 1L
    cat("PASS", label, "\n")
  } else {
    fail <<- fail + 1L
    cat("FAIL", label, "\n")
  }
}

month <- function() {
  data.frame(
    periode = "2026-10",
    REV = 1000, OTHER_OP_INC = 0, COGS = 400, PAYROLL = 200, OPEX_OTHER = 100,
    D_A = 50, FIN = 0, EXCEP = 0, TAX = 0,
    stringsAsFactors = FALSE
  )
}
ranges <- data.frame(
  id = c("cout.variable.pct", "dso"),
  label = c("Coût variable", "Délai clients (jours)"),
  low_pct = c(15, 15),
  high_pct = c(20, 20),
  stringsAsFactors = FALSE
)
edits <- list("2026-10" = list(occ = 70, rate = 120, head = 4, cost = 3000))

high <- fpa_shiny_case_pnl(month(), NULL, "2026-10", edits, NA, ranges, "high", NULL)
low <- fpa_shiny_case_pnl(month(), NULL, "2026-10", edits, NA, ranges, "low", NULL)
base <- fpa_shiny_case_pnl(month(), NULL, "2026-10", edits, NA, ranges, "base", NULL)

check("high leaves revenue", abs(high$REV[[1]] - 1000) < 0.5)
check("high cuts variable cost 20%", abs(high$COGS[[1]] - 320) < 0.5)
check("low raises variable cost 15%", abs(low$COGS[[1]] - 460) < 0.5)
check("base keeps the plan", abs(base$COGS[[1]] - 400) < 0.5)
check("high and low operating income differ", abs(high$OI[[1]] - low$OI[[1]]) > 1)
check("client days do not move revenue", abs(high$REV[[1]] - low$REV[[1]]) < 0.5)

bare <- fpa_shiny_case_pnl(month(), NULL, "2026-10", list(), NA, ranges, "high", NULL, active = "2026-10")
check("no driver form still applies the percent", abs(bare$COGS[[1]] - 320) < 0.5)

vol <- data.frame(periode = "2026-10", capacite = 10, stringsAsFactors = FALSE)
driven <- fpa_shiny_case_pnl(month(), vol, "2026-10", edits, NA, ranges, "high", NULL)
check("occupancy rewrite keeps the rooms identity", abs(driven$REV[[1]] - 10 * 0.70 * 120) < 0.5)
check("variable cost still moves beside that rewrite", abs(driven$COGS[[1]] - 320) < 0.5)

cat("PASS", pass, "FAIL", fail, "\n")
quit(status = if (fail) 1 else 0)
