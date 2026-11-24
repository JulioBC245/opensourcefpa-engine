# SPDX-License-Identifier: AGPL-3.0-or-later
# Copyright (C) 2026 Julio Bauer Cantuaria
# Bottom-line battery: every graded series against operating income.
# Same window, same 2 000 reshuffles, seed 21. Writes the live grade file.
root <- "."
setwd(root)
source("tests/helpers.R", encoding = "UTF-8")
fpa_test_bootstrap()
parse_app <- parse(file.path(root, "app.R"))
stopifnot(length(parse_app) > 0)

base <- "../fpa-local-test"
config <- fpa_load_config(file.path(base, "fpa-config.json"))
grades_path <- file.path(base, "work/drivers/driver-grades.csv")
grades <- utils::read.csv(grades_path, stringsAsFactors = FALSE)
filled <- fpa_driver_apply_bottom(grades, config)

fail <- 0L
ok <- 0L
check <- function(cond, label) {
  if (isTRUE(cond)) {
    ok <<- ok + 1L
    cat("PASS", label, "\n")
  } else {
    fail <<- fail + 1L
    cat("FAIL", label, "\n")
  }
}

row_of <- function(name) filled[filled$series_name == name, , drop = FALSE]
oi_n <- sum(filled$bottom_line == "OI")
check(oi_n == nrow(filled) && oi_n >= 8L, "every series tested against OI")

nights <- row_of("nuitees_vendues")
check(nrow(nights) == 1L && abs(nights$bottom_r - 0.521) < 0.01, "nights r about 0.52")
check(nights$bottom_lag == -2L, "nights lag -2")
check(nights$bottom_rank <= 0.05 / oi_n, "nights clears the joint bar")
check(abs(nights$bottom_partial - 0.578) < 0.02, "nights partial after rate")
check(identical(as.character(nights$bottom_control), "tarif"), "nights controlled for tarif")
check(all(!is.finite(filled$bottom_eur10)), "no euro read from a correlation")

tarif <- row_of("tarif")
check(tarif$bottom_lag == 0L, "tarif same month")
check(is.finite(tarif$bottom_rank) && tarif$bottom_rank <= 0.05, "tarif clears 5%")
check(identical(as.character(tarif$bottom_control), "nuitees_vendues"), "tarif controlled for nights")
check(tarif$bottom_rank <= 0.05 / oi_n, "tarif clears the joint bar")

cap <- row_of("capacite")
check(cap$bottom_line == "OI" && !is.finite(cap$bottom_r), "capacity has no correlation")

eff <- row_of("effectif")
check(is.finite(eff$bottom_r) && eff$bottom_r < 0 && eff$bottom_lag == 2L, "headcount leads and is negative")

temp <- row_of("temperature_c")
check(is.finite(temp$bottom_rank) && temp$bottom_rank > 0.5, "temperature does not clear")

rain <- row_of("precipitations_mm")
check(is.finite(rain$bottom_rank) && rain$bottom_rank > 0.5, "rain does not clear")

check(all(nzchar(filled$bottom_line)), "battery marked done")

if (fail == 0L) fpa_driver_write_csv(filled, grades_path)
cat("RESULT", ok, "pass", fail, "fail\n")
quit(status = if (fail == 0L) 0L else 1L)
