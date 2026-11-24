# SPDX-License-Identifier: AGPL-3.0-or-later
# Copyright (C) 2026 Julio Bauer Cantuaria
# Opening BS named vectors omit AP/AR/INV; [[key]] must not throw.
source("R/fpa-config.R", encoding = "UTF-8")
source("R/fpa-wc.R", encoding = "UTF-8")

opening <- c(CASH = 100, AR = 10, EQUITY = 90)
stopifnot(identical(fpa_open_num(opening, "AP"), 0))
stopifnot(identical(fpa_open_num(opening, "AR"), 10))
stopifnot(identical(fpa_open_num(NULL, "CASH"), 0))
stopifnot(identical(fpa_open_num(numeric(0), "AP"), 0))

periods <- c("2026-01", "2026-02")
wc <- list(
  AR = setNames(c(10, 12), periods),
  AP = setNames(c(0, 1), periods),
  INV = setNames(c(0, 0), periods)
)
impact <- fpa_wc_cash_impact(wc, periods, opening)
stopifnot(length(impact) == 2, all(is.finite(impact)))

message("PASS open-num")
