# SPDX-License-Identifier: AGPL-3.0-or-later
# Copyright (C) 2026 Julio Bauer Cantuaria
# Seasonal level: an open month follows prior years, scaled by this year's actuals.
source("tests/helpers.R", encoding = "UTF-8")
fpa_test_bootstrap()

shape <- c(10, 10, 10, 10, 10, 10, 40, 40, 20, 8, 8, 8)
hist <- data.frame(
  periode = c(sprintf("2024-%02d", 1:12), sprintf("2025-%02d", 1:12), sprintf("2026-%02d", 1:8)),
  REV = c(shape, shape, shape[1:8] * 1.2),
  stringsAsFactors = FALSE
)
pnl <- hist
actual <- sprintf("2026-%02d", 1:8)
got <- fpa_seasonal_month(pnl, hist, "REV", "2026-10", actual)
stopifnot(abs(got - 9.6) < 1e-6)

pnl2 <- rbind(pnl, data.frame(periode = "2026-10", REV = 0, stringsAsFactors = FALSE))
filled <- fpa_pnl_apply_seasonal_level(pnl2, hist, "2026-10", actual, "REV")
stopifnot(abs(filled$REV[filled$periode == "2026-10"] - 9.6) < 1e-6)
kept <- fpa_pnl_apply_seasonal_level(pnl2, hist, "2026-10", actual, "REV")
# a non-zero forecast month is left alone
pnl2$REV[pnl2$periode == "2026-10"] <- 50
kept <- fpa_pnl_apply_seasonal_level(pnl2, hist, "2026-10", actual, "REV")
stopifnot(abs(kept$REV[kept$periode == "2026-10"] - 50) < 1e-6)

# When the statement and a parallel history disagree, the month on the statement wins.
hist_other <- hist
hist_other$REV[hist_other$periode == "2026-07"] <- 29089
copied <- fpa_seasonal_month(pnl, hist_other, "REV", "2027-07", actual)
stopifnot(abs(copied - pnl$REV[pnl$periode == "2026-07"]) < 1e-6)

message("PASS season")
