# SPDX-License-Identifier: AGPL-3.0-or-later
# Copyright (C) 2026 Julio Bauer Cantuaria
# Month / YYMM / MM period labels used by rolling P&L files.
# LAST CHANGE: 2026-09-21
source("tests/helpers.R", encoding = "UTF-8")
root <- fpa_test_bootstrap()

stopifnot(identical(fpa_parse_period("2024-01"), as.Date("2024-01-31")))
stopifnot(identical(fpa_parse_period("2024-01-15"), as.Date("2024-01-15")))
stopifnot(identical(fpa_parse_period("2024"), as.Date("2024-12-31")))
stopifnot(identical(fpa_parse_period("202401"), as.Date("2024-01-31")))
stopifnot(identical(fpa_parse_period("2401"), as.Date("2024-01-31")))

jan <- fpa_parse_period_one("Janvier", year_hint = 2026L)
stopifnot(identical(jan, as.Date("2026-01-31")))
fev <- fpa_parse_period_one("Février", year_hint = 2026L)
stopifnot(identical(fev, as.Date("2026-02-28")))
feb <- fpa_parse_period_one("Feb", year_hint = 2026L)
stopifnot(identical(feb, as.Date("2026-02-28")))
mm <- fpa_parse_period_one("01", year_hint = 2026L)
stopifnot(identical(mm, as.Date("2026-01-31")))

vec <- fpa_parse_period(c("Janvier", "janv-24", "01/2024"))
stopifnot(identical(vec[[2]], as.Date("2024-01-31")))
stopifnot(identical(vec[[3]], as.Date("2024-01-31")))
# Yearless January borrows 2024 from the dated neighbours.
stopifnot(identical(vec[[1]], as.Date("2024-01-31")))

message("test-period-parse ok")
